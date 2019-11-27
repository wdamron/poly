// The MIT License (MIT)
//
// Copyright (c) 2019 West Damron
//
// Permission is hereby granted, free of charge, to any person obtaining a copy
// of this software and associated documentation files (the "Software"), to deal
// in the Software without restriction, including without limitation the rights
// to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
// copies of the Software, and to permit persons to whom the Software is
// furnished to do so, subject to the following conditions:
//
// The above copyright notice and this permission notice shall be included in all
// copies or substantial portions of the Software.
//
// THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
// IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
// FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
// AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
// LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
// OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
// SOFTWARE.

package astutil

import (
	"errors"

	"github.com/wdamron/poly/ast"
)

// Analysis for grouped let-bindings which may be mutually-recursive; borrowed from Haskell.
// https://prime.haskell.org/wiki/RelaxedDependencyAnalysis
//
//   In Haskell 98, a group of bindings is sorted into strongly-connected components, and then type-checked
//   in dependency order (​H98 s4.5.1). As each dependency group is type-checked, any binders of the group
//   that have an explicit type signature are put in the type environment with the specified polymorphic type,
//   and all others are monomorphic until the group is generalized (​H98 s4.5.2).
//
//   The initial dependency analysis should ignore references to variables that have an explicit type signature.
type Analysis struct {
	Scopes      map[string]int // map from variable to let-group number (or -1 for variables not bound by let-groups)
	ScopeStash  []StashedScope // shadowed variable-scope mappings
	Graphs      []graph        // indexed by let-group number
	CurrentVert []int          // indexed by let-group number
	Sccs        [][][]int      // indexed by let-group number
	Err         error
	Invalid     ast.Expr

	// initial space:
	_scopeStash  [16]StashedScope
	_graphs      [16]graph
	_currentVert [16]int
	_sccs        [16][][]int
}

type StashedScope struct {
	Name     string
	GroupNum int
}

func (a *Analysis) Init() {
	a.Scopes = make(map[string]int, 32)
	a.ScopeStash, a.Graphs, a.CurrentVert, a.Sccs =
		a._scopeStash[:0], a._graphs[:0], a._currentVert[:0], a._sccs[:0]
}

func (a *Analysis) Reset() {
	for v := range a.Scopes {
		delete(a.Scopes, v)
	}
	for i := range a._scopeStash {
		a._scopeStash[i] = StashedScope{}
	}
	for i := range a._graphs {
		a._graphs[i] = graph{}
	}
	for i := range a._currentVert {
		a._currentVert[i] = -1
	}
	for i := range a._sccs {
		a._sccs[i] = nil
	}
	a.ScopeStash, a.Graphs, a.CurrentVert, a.Sccs, a.Err, a.Invalid =
		a._scopeStash[:0], a._graphs[:0], a._currentVert[:0], nil, nil, nil
}

func (a *Analysis) Analyze(root ast.Expr) error {
	if err := a.analyzeExpr(root); err != nil {
		a.Err = err
		return err
	}
	ng := len(a.Graphs)
	if ng > len(a._sccs) {
		a.Sccs = make([][][]int, ng)
	} else {
		a.Sccs = a._sccs[:ng]
	}
	for groupNum := range a.Graphs {
		a.Sccs[groupNum] = tarjanSCC(&a.Graphs[groupNum])
		// Reverse the order for a topological sort:
		sccs := a.Sccs[groupNum]
		for i, j := 0, len(sccs)-1; i < j; i, j = i+1, j-1 {
			sccs[i], sccs[j] = sccs[j], sccs[i]
		}
	}
	return nil
}

// returns 1 if the variable was stashed, otherwise 0
func (a *Analysis) stash(name string) int {
	if groupNum, exists := a.Scopes[name]; exists {
		a.ScopeStash = append(a.ScopeStash, StashedScope{name, groupNum})
		return 1
	}
	return 0
}

func (a *Analysis) unstash(count int) {
	if count <= 0 {
		return
	}
	stash := a.ScopeStash
	unstashed := 0
	for i := len(stash) - 1; unstashed < count && i >= 0; i, unstashed = i-1, unstashed+1 {
		a.Scopes[stash[i].Name] = stash[i].GroupNum
	}
	a.ScopeStash = a.ScopeStash[0 : len(stash)-unstashed]
}

func (a *Analysis) analyzeExpr(expr ast.Expr) error {
	switch expr := expr.(type) {
	case *ast.Literal:
		for _, name := range expr.Using {
			if groupNum, ok := a.Scopes[name]; ok && groupNum >= 0 {
				graph := &a.Graphs[groupNum]
				if a.CurrentVert[groupNum] >= 0 {
					graph.addEdge(graph.verts[name], a.CurrentVert[groupNum])
				}
			}
		}

	case *ast.Var:
		if groupNum, ok := a.Scopes[expr.Name]; ok && groupNum >= 0 {
			graph := &a.Graphs[groupNum]
			if a.CurrentVert[groupNum] >= 0 {
				graph.addEdge(graph.verts[expr.Name], a.CurrentVert[groupNum])
			}
		}

	case *ast.Deref:
		if err := a.analyzeExpr(expr.Ref); err != nil {
			return err
		}

	case *ast.DerefAssign:
		if err := a.analyzeExpr(expr.Ref); err != nil {
			return err
		}
		if err := a.analyzeExpr(expr.Value); err != nil {
			return err
		}

	case *ast.Pipe:
		if err := a.analyzeExpr(expr.Source); err != nil {
			return err
		}
		stashed := a.stash(expr.As)
		a.Scopes[expr.As] = -1
		for _, sub := range expr.Sequence {
			if err := a.analyzeExpr(sub); err != nil {
				return err
			}
		}
		delete(a.Scopes, expr.As)
		a.unstash(stashed)

	case *ast.ControlFlow:
		stashed := 0
		for _, local := range expr.Locals {
			stashed += a.stash(local)
			a.Scopes[local] = -1
		}
		for _, sub := range expr.Entry.Sequence {
			if err := a.analyzeExpr(sub); err != nil {
				return err
			}
		}
		for _, sub := range expr.Return.Sequence {
			if err := a.analyzeExpr(sub); err != nil {
				return err
			}
		}
		for _, block := range expr.Blocks {
			for _, sub := range block.Sequence {
				if err := a.analyzeExpr(sub); err != nil {
					return err
				}
			}
		}
		for _, local := range expr.Locals {
			delete(a.Scopes, local)
		}
		a.unstash(stashed)

	case *ast.Call:
		if err := a.analyzeExpr(expr.Func); err != nil {
			return err
		}
		for _, arg := range expr.Args {
			if err := a.analyzeExpr(arg); err != nil {
				return err
			}
		}

	case *ast.Func:
		stashed := 0
		for _, name := range expr.ArgNames {
			stashed += a.stash(name)
			a.Scopes[name] = -1
		}
		if err := a.analyzeExpr(expr.Body); err != nil {
			return err
		}
		for _, name := range expr.ArgNames {
			delete(a.Scopes, name)
		}
		a.unstash(stashed)

	case *ast.Let:
		stashed := 0
		_, isFunc := expr.Value.(*ast.Func)
		// Allow self-references within function types:
		if isFunc {
			stashed = a.stash(expr.Var)
			a.Scopes[expr.Var] = -1
		}
		if err := a.analyzeExpr(expr.Value); err != nil {
			return err
		}
		if !isFunc {
			stashed = a.stash(expr.Var)
			a.Scopes[expr.Var] = -1
		}
		if err := a.analyzeExpr(expr.Body); err != nil {
			return err
		}
		delete(a.Scopes, expr.Var)
		a.unstash(stashed)

	case *ast.LetGroup:
		num := len(a.Graphs)
		a.Graphs = append(a.Graphs, graph{
			verts: make(map[string]int, len(expr.Vars)),
			edges: make([][]int, len(expr.Vars)),
		})
		a.CurrentVert = append(a.CurrentVert, -1)
		graph := &a.Graphs[num]
		stashed := 0
		for _, v := range expr.Vars {
			if !graph.addVert(v.Var) {
				a.Invalid = expr
				return errors.New("Found duplicate bindings for " + v.Var + " within let-group")
			}
			stashed += a.stash(v.Var)
			a.Scopes[v.Var] = num
		}
		for i, v := range expr.Vars {
			a.CurrentVert[num] = i
			// Allow self-references within function types:
			if _, isFunc := v.Value.(*ast.Func); isFunc {
				if err := a.analyzeExpr(v.Value); err != nil {
					return err
				}
				continue
			}
			// Disallow self-references within non-function types:
			exists := false
			for i := 0; i < stashed; i++ {
				existing := a.ScopeStash[len(a.ScopeStash)-(1+i)]
				if existing.Name == v.Var {
					a.Scopes[v.Var] = existing.GroupNum
					exists = true
					break
				}
			}
			if !exists {
				delete(a.Scopes, v.Var)
			}
			if err := a.analyzeExpr(v.Value); err != nil {
				return err
			}
			a.Scopes[v.Var] = num
		}
		a.CurrentVert[num] = -1
		if err := a.analyzeExpr(expr.Body); err != nil {
			return err
		}
		for _, v := range expr.Vars {
			delete(a.Scopes, v.Var)
		}
		a.unstash(stashed)

	case *ast.RecordSelect:
		if err := a.analyzeExpr(expr.Record); err != nil {
			return err
		}

	case *ast.RecordExtend:
		if err := a.analyzeExpr(expr.Record); err != nil {
			return err
		}
		for _, label := range expr.Labels {
			if err := a.analyzeExpr(label.Value); err != nil {
				return err
			}
		}

	case *ast.RecordRestrict:
		if err := a.analyzeExpr(expr.Record); err != nil {
			return err
		}

	case *ast.RecordEmpty:
		// nothing to check

	case *ast.Variant:
		if err := a.analyzeExpr(expr.Value); err != nil {
			return err
		}

	case *ast.Match:
		if err := a.analyzeExpr(expr.Value); err != nil {
			return err
		}
		for _, c := range expr.Cases {
			stashed := a.stash(c.Var)
			a.Scopes[c.Var] = -1
			if err := a.analyzeExpr(c.Value); err != nil {
				return err
			}
			delete(a.Scopes, c.Var)
			a.unstash(stashed)
		}
		if expr.Default != nil {
			c := expr.Default
			stashed := a.stash(c.Var)
			a.Scopes[c.Var] = -1
			if err := a.analyzeExpr(c.Value); err != nil {
				return err
			}
			delete(a.Scopes, c.Var)
			a.unstash(stashed)
		}

	case nil:
		return errors.New("Failed to analyze nil expression")

	default:
		return errors.New("Failed to analyze " + expr.ExprName() + " expression")
	}

	return nil
}

// Tarjan's SCC algorithm, based on https://github.com/gonum/gonum/blob/master/graph/topo/tarjan.go
//
// Components will be output in reversed dependency-order. Reversing the output creates a proper topological sort.

type graph struct {
	verts map[string]int
	edges [][]int
}

func (g *graph) addVert(name string) bool {
	if _, ok := g.verts[name]; ok {
		return false
	}
	g.verts[name] = len(g.verts)
	return true
}

func (g *graph) addEdge(from, to int) {
	for _, succ := range g.edges[from] {
		if succ == to {
			return
		}
	}
	g.edges[from] = append(g.edges[from], to)
}

type sccState struct {
	index      int
	indexTable []int
	lowLink    []int
	onStack    []bool

	stack []int
	sccs  [][]int
}

func tarjanSCC(g *graph) [][]int {
	state := sccState{
		indexTable: make([]int, len(g.edges)),
		lowLink:    make([]int, len(g.edges)),
		onStack:    make([]bool, len(g.edges)),
	}
	for v := range g.edges {
		if state.indexTable[v] == 0 {
			state.tarjanSCC(g, v)
		}
	}
	return state.sccs
}

func (state *sccState) tarjanSCC(g *graph, v int) {
	min := func(a, b int) int {
		if a < b {
			return a
		}
		return b
	}
	// Set the depth index for v to the smallest unused index
	state.index++
	state.indexTable[v] = state.index
	state.lowLink[v] = state.index
	state.stack = append(state.stack, v)
	state.onStack[v] = true

	// Consider successors of v
	for _, succ := range g.edges[v] {
		if state.indexTable[succ] == 0 {
			// Successor has not yet been visited; recur on it
			state.tarjanSCC(g, succ)
			state.lowLink[v] = min(state.lowLink[v], state.lowLink[succ])
		} else if state.onStack[succ] {
			// Successor is in stack s and hence in the current SCC
			state.lowLink[v] = min(state.lowLink[v], state.indexTable[succ])
		}
	}

	// If v is a root node, pop the stack and generate an SCC
	if state.lowLink[v] == state.indexTable[v] {
		// Start a new strongly connected component
		var (
			c    []int
			succ int
		)
		for {
			succ, state.stack = state.stack[len(state.stack)-1], state.stack[:len(state.stack)-1]
			state.onStack[succ] = false
			// Add successor to current strongly connected component
			c = append(c, succ)
			if succ == v {
				break
			}
		}
		// Output the current strongly connected component
		state.sccs = append(state.sccs, c)
	}
}

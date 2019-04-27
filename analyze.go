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

package poly

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
type analysis struct {
	groupNums   map[*ast.LetGroup]int
	scopes      map[string]int // map from variable to let-group number (or -1 for function-bound variables)
	scopeStash  []stashedScope // shadowed variable-scope mappings
	graphs      []graph        // indexed by let-group number
	currentVert []int          // indexed by let-group number
	sccs        [][][]int      // indexed by let-group number
	err         error
	invalid     ast.Expr

	// initial space:
	_scopeStash  [16]stashedScope
	_graphs      [16]graph
	_currentVert [16]int
	_sccs        [16][][]int
}

type stashedScope struct {
	Name     string
	GroupNum int
}

func (a *analysis) init() {
	a.scopes = make(map[string]int, 32)
	a.groupNums = make(map[*ast.LetGroup]int, 16)
	a.scopeStash, a.graphs, a.currentVert, a.sccs =
		a._scopeStash[:0], a._graphs[:0], a._currentVert[:0], a._sccs[:0]
}

func (a *analysis) reset() {
	for g := range a.groupNums {
		delete(a.groupNums, g)
	}
	for v := range a.scopes {
		delete(a.scopes, v)
	}
	for i := range a._scopeStash {
		a._scopeStash[i] = stashedScope{}
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
	a.scopeStash, a.graphs, a.currentVert, a.sccs, a.err, a.invalid =
		a._scopeStash[:0], a._graphs[:0], a._currentVert[:0], a._sccs[:0], nil, nil
}

// returns 1 if the variable was stashed, otherwise 0
func (a *analysis) stash(name string) int {
	if groupNum, exists := a.scopes[name]; exists {
		a.scopeStash = append(a.scopeStash, stashedScope{name, groupNum})
		return 1
	}
	return 0
}

func (a *analysis) unstash(count int) {
	if count <= 0 {
		return
	}
	stash := a.scopeStash
	unstashed := 0
	for i := len(stash) - 1; unstashed < count && i >= 0; i, unstashed = i-1, unstashed+1 {
		a.scopes[stash[i].Name] = stash[i].GroupNum
	}
	a.scopeStash = a.scopeStash[0 : len(stash)-unstashed]
}

func (a *analysis) analyze(root ast.Expr) error {
	if err := a.analyzeExpr(root); err != nil {
		a.err = err
		return err
	}
	a.sccs = make([][][]int, len(a.graphs))
	for groupNum := range a.graphs {
		a.sccs[groupNum] = tarjanSCC(&a.graphs[groupNum])
		// Reverse the order for a topological sort:
		sccs := a.sccs[groupNum]
		for i, j := 0, len(sccs)-1; i < j; i, j = i+1, j-1 {
			sccs[i], sccs[j] = sccs[j], sccs[i]
		}
	}
	return nil
}

func (a *analysis) analyzeExpr(expr ast.Expr) error {
	switch expr := expr.(type) {
	case *ast.Var:
		if groupNum, ok := a.scopes[expr.Name]; ok && groupNum >= 0 {
			graph := &a.graphs[groupNum]
			if a.currentVert[groupNum] >= 0 {
				graph.addEdge(graph.verts[expr.Name], a.currentVert[groupNum])
			}
		}

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
			a.scopes[name] = -1
		}
		if err := a.analyzeExpr(expr.Body); err != nil {
			return err
		}
		for _, name := range expr.ArgNames {
			delete(a.scopes, name)
		}
		a.unstash(stashed)

	case *ast.Let:
		stashed := 0
		_, isFunc := expr.Value.(*ast.Func)
		// Allow self-references within function types:
		if isFunc {
			stashed = a.stash(expr.Var)
			a.scopes[expr.Var] = -1
		}
		if err := a.analyzeExpr(expr.Value); err != nil {
			return err
		}
		if !isFunc {
			stashed = a.stash(expr.Var)
			a.scopes[expr.Var] = -1
		}
		if err := a.analyzeExpr(expr.Body); err != nil {
			return err
		}
		delete(a.scopes, expr.Var)
		a.unstash(stashed)

	case *ast.LetGroup:
		if _, exists := a.groupNums[expr]; exists {
			a.invalid = expr
			return errors.New("non-unique nested let-group pointer")
		}
		num := len(a.groupNums)
		a.groupNums[expr] = num
		a.graphs = append(a.graphs, graph{
			verts: make(map[string]int, len(expr.Vars)),
			edges: make([][]int, len(expr.Vars)),
		})
		a.currentVert = append(a.currentVert, -1)
		graph := &a.graphs[num]
		stashed := 0
		for _, v := range expr.Vars {
			if !graph.addVert(v.Var) {
				a.invalid = expr
				return errors.New("non-unique binding names for " + v.Var + " within let-group")
			}
			stashed += a.stash(v.Var)
			a.scopes[v.Var] = num
		}
		for i, v := range expr.Vars {
			a.currentVert[num] = i
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
				existing := a.scopeStash[len(a.scopeStash)-(1+i)]
				if existing.Name == v.Var {
					a.scopes[v.Var] = existing.GroupNum
					exists = true
					break
				}
			}
			if !exists {
				delete(a.scopes, v.Var)
			}
			if err := a.analyzeExpr(v.Value); err != nil {
				return err
			}
			a.scopes[v.Var] = num
		}
		a.currentVert[num] = -1
		if err := a.analyzeExpr(expr.Body); err != nil {
			return err
		}
		for _, v := range expr.Vars {
			delete(a.scopes, v.Var)
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
			a.scopes[c.Var] = -1
			if err := a.analyzeExpr(c.Value); err != nil {
				return err
			}
			delete(a.scopes, c.Var)
			a.unstash(stashed)
		}
		if expr.Default != nil {
			c := expr.Default
			stashed := a.stash(c.Var)
			a.scopes[c.Var] = -1
			if err := a.analyzeExpr(c.Value); err != nil {
				return err
			}
			delete(a.scopes, c.Var)
			a.unstash(stashed)
		}
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

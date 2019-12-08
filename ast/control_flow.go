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

package ast

import (
	"errors"
	"sort"
	"sync"

	"github.com/wdamron/poly/internal/util"
	"github.com/wdamron/poly/types"
)

var _ Expr = (*ControlFlow)(nil)

var (
	ControlFlowEntryIndex  = -2
	ControlFlowReturnIndex = -1
)

// Block is an expression sequence within a control-flow graph.
type Block struct {
	// Index of the block within the Blocks slice of a control-flow graph
	Index int
	// The last expression in Sequence is the result type for the block
	Sequence []Expr
}

// Check if b is the entry block for a control-flow graph.
func (b *Block) IsEntry() bool { return b.Index == ControlFlowEntryIndex }

// Check if b is the return block for a control-flow graph.
func (b *Block) IsReturn() bool { return b.Index == ControlFlowReturnIndex }

// Jump is a "goto" between a pair of blocks in a control-flow graph.
type Jump struct {
	// From and To are block indexes
	From, To int
}

// Check if j is a jump from the entry block.
func (j *Jump) FromEntry() bool { return j.From == ControlFlowEntryIndex }

// Check if j is a jump from the return block (not valid control flow).
func (j *Jump) FromReturn() bool { return j.From == ControlFlowReturnIndex }

// Check if j is a jump to the entry block (not valid control flow).
func (j *Jump) ToEntry() bool { return j.To == ControlFlowEntryIndex }

// Check if j is a jump to the return block.
func (j *Jump) ToReturn() bool { return j.To == ControlFlowReturnIndex }

// Control-flow graph
type ControlFlow struct {
	Name string
	// Locals are shared variable names used within the control flow expression,
	// bound as mutable reference types during inference
	Locals []string
	Entry  Block
	// Return determines the result type of a control flow expression
	Return   Block
	Blocks   []Block
	Jumps    []Jump
	sccs     [][]Block
	inferred types.Type
}

// Create a new control flow expression.
func NewControlFlow(name string, locals ...string) *ControlFlow {
	return &ControlFlow{
		Name:   name,
		Locals: locals,
		Entry:  Block{Index: ControlFlowEntryIndex},
		Return: Block{Index: ControlFlowReturnIndex},
	}
}

// "ControlFlow"
func (e *ControlFlow) ExprName() string { return "ControlFlow" }

// Get the inferred (or assigned) type of e.
func (e *ControlFlow) Type() types.Type { return types.RealType(e.inferred) }

// Assign a type to e. Type assignments should occur indirectly, during inference.
func (e *ControlFlow) SetType(t types.Type) { e.inferred = t }

// Check if e has been validated and not modified since the last validation.
func (e *ControlFlow) IsValidated() bool { return len(e.sccs) != 0 }

// Assign an expression sequence to the entry block for e.
func (e *ControlFlow) SetEntry(block ...Expr) {
	e.Entry.Sequence, e.sccs = block, nil
}

// Assign an expression sequence to the return block for e. The return block determines
// the result type of a control flow expression.
func (e *ControlFlow) SetReturn(block ...Expr) {
	e.Return.Sequence, e.sccs = block, nil
}

// Add a new block to e. Each block should contain a sequence of 1 or more expressions.
func (e *ControlFlow) AddBlock(block ...Expr) Block {
	e.Blocks, e.sccs = append(e.Blocks, Block{len(e.Blocks), block}), nil
	return e.Blocks[len(e.Blocks)-1]
}

// Add a jump for a pair of blocks. The return block determines the result type of a control flow expression.
func (e *ControlFlow) AddJump(from, to Block) {
	if !e.HasJump(from, to) {
		e.Jumps, e.sccs = append(e.Jumps, Jump{from.Index, to.Index}), nil
	}
}

// Check if e has a jump for a given pair of blocks.
func (e *ControlFlow) HasJump(from, to Block) bool {
	for _, j := range e.Jumps {
		if j.From == from.Index && j.To == to.Index {
			return true
		}
	}
	return false
}

// Ensure the entry and return blocks are not within cycles and all blocks and cycles in the strongly
// connected components for e reach the return block, directly or transitively.
//
// The strongly connected components of the control-flow graph will be returned in dependency order.
//
// If annotate is true, the strongly connected componenents will be cached in e.
// If e has already been validated with annotation enabled, a cached result will be returned.
func (e *ControlFlow) Validate(annotate bool) (sccs [][]Block, err error) {
	if len(e.sccs) != 0 {
		return e.sccs, nil
	}
	// Loops are detected through SCC analysis and inferred as recursive functions.
	// Blocks are inferred in dependency order.
	sccs = e.StronglyConnectedComponents()
	if len(sccs) < 2 {
		return nil, errors.New("Control flow must reach the entry and return blocks")
	}
	if len(sccs[0]) != 1 || sccs[0][0].Index != ControlFlowEntryIndex {
		return nil, errors.New("Control flow must begin at a non-cyclical entry block")
	}
	if len(sccs[len(sccs)-1]) != 1 || sccs[len(sccs)-1][0].Index != ControlFlowReturnIndex {
		return nil, errors.New("Control flow must end at a non-cyclical return block")
	}
	// Ensure all blocks and cycles in the strongly connected components for e reach the return block,
	// directly or transitively:
	if err = e.checkReturnReachability(sccs); err != nil {
		return nil, err
	}
	if annotate {
		e.sccs = sccs
	}
	return sccs, nil
}

const smallGraphSize = 16

type smallGraph [smallGraphSize][]int

var graphPool = sync.Pool{
	New: func() interface{} { return new(smallGraph) },
}

func (g *smallGraph) Release() {
	for i := range g {
		g[i] = nil
	}
	graphPool.Put(g)
}

// Compute the strongly connected components of a control-flow graph.
//
// Cycles of blocks will be returned in dependency order.
// Cycles containing a single block may or may not be a cycle; a block is a cycle
// if the control-flow graph has a jump from that block to itself.
//
// If e has already been validated with annotation enabled, a cached result will be returned.
func (e *ControlFlow) StronglyConnectedComponents() [][]Block {
	if len(e.sccs) != 0 {
		return e.sccs
	}
	var (
		graph util.Graph
		small *smallGraph
	)
	numVerts := len(e.Blocks) + 2 // extra space is added for the entry and return
	if numVerts > smallGraphSize {
		graph = util.NewGraph(numVerts)
	} else {
		small = graphPool.Get().(*smallGraph)
		graph = small[:numVerts]
	}
	for _, j := range e.Jumps {
		// Map block indexes to (non-negative) temporary indexes:
		from, to := j.From, j.To
		if from < 0 {
			from = len(graph) + from
		}
		if to < 0 {
			to = len(graph) + to
		}
		graph.AddEdge(from, to)
	}
	scc := graph.SCC()
	if small != nil {
		small.Release()
	}
	cycles := make([][]Block, len(scc))
	for i, vs := range scc {
		// Map temporary indexes back to blocks:
		cycle := make([]Block, len(vs))
		for j, v := range vs {
			switch v {
			case len(graph) + ControlFlowEntryIndex:
				cycle[j] = e.Entry
			case len(graph) + ControlFlowReturnIndex:
				cycle[j] = e.Return
			default:
				cycle[j] = e.Blocks[v]
			}
		}
		cycles[i] = cycle
	}
	return cycles
}

// Ensure all blocks and cycles in the strongly connected components for e reach the return block,
// directly or transitively.
func (e *ControlFlow) checkReturnReachability(stronglyConnectedComponents [][]Block) (err error) {
	if len(e.Blocks) <= 64 {
		return e.checkReturnReachabilitySmall(stronglyConnectedComponents)
	}
	return e.checkReturnReachabilityLarge(stronglyConnectedComponents)
}

func (e *ControlFlow) checkReturnReachabilityLarge(stronglyConnectedComponents [][]Block) (err error) {
	reachesReturn, entryReachesReturn, jumpsMarked := make([]bool, len(e.Blocks)), false, 0
	for {
		changed := false
		for _, jump := range e.Jumps {
			if jump.FromReturn() || jump.ToEntry() {
				return errors.New("Control flow must not jump from the return block or to the entry block")
			}
			toReturn := jump.ToReturn() || reachesReturn[jump.To]
			if jump.FromEntry() {
				if !entryReachesReturn && toReturn {
					entryReachesReturn = true
					changed = true
					jumpsMarked++
				}
				continue
			}
			if !reachesReturn[jump.From] && toReturn {
				reachesReturn[jump.From] = true
				changed = true
				jumpsMarked++
			}
		}
		if !changed || jumpsMarked == len(e.Jumps) {
			break
		}
	}
	for _, cycle := range stronglyConnectedComponents {
		for _, block := range cycle {
			if block.IsEntry() {
				if entryReachesReturn {
					continue
				}
			} else if block.IsReturn() || reachesReturn[block.Index] {
				continue
			}
			return errors.New("Control flow contains blocks which do not reach the return block")
		}
	}
	return nil
}

func (e *ControlFlow) checkReturnReachabilitySmall(stronglyConnectedComponents [][]Block) (err error) {
	hasBit := func(bits uint64, bit int) bool { return bits&(1<<uint(bit)) != 0 }
	setBit := func(bits uint64, bit int) uint64 { return bits | (1 << uint(bit)) }
	reachesReturn, entryReachesReturn, jumpsMarked := uint64(0), false, 0
	for {
		changed := false
		for _, jump := range e.Jumps {
			if jump.FromReturn() || jump.ToEntry() {
				return errors.New("Control flow must not jump from the return block or to the entry block")
			}
			toReturn := jump.ToReturn() || hasBit(reachesReturn, jump.To)
			if jump.FromEntry() {
				if !entryReachesReturn && toReturn {
					entryReachesReturn = true
					changed = true
					jumpsMarked++
				}
				continue
			}
			if !hasBit(reachesReturn, jump.From) && toReturn {
				reachesReturn = setBit(reachesReturn, jump.From)
				changed = true
				jumpsMarked++
			}
		}
		if !changed || jumpsMarked == len(e.Jumps) {
			break
		}
	}
	for _, cycle := range stronglyConnectedComponents {
		for _, block := range cycle {
			if block.IsEntry() {
				if entryReachesReturn {
					continue
				}
			} else if block.IsReturn() || hasBit(reachesReturn, block.Index) {
				continue
			}
			return errors.New("Control flow contains blocks which do not reach the return block")
		}
	}
	return nil
}

// Copy and sort jumps in ascending order by {From, To}.
func SortJumps(jumps []Jump) []Jump {
	less := func(i, j int) bool {
		return jumps[i].From < jumps[j].From || (jumps[i].From == jumps[j].From && jumps[i].To < jumps[j].To)
	}
	if len(jumps) == 1 {
		return jumps
	}
	if len(jumps) == 2 {
		lt, gt := less(0, 1), less(1, 0)
		if !(lt || !gt) { // not less than or equal
			return []Jump{jumps[1], jumps[0]}
		}
		return jumps
	}
	sorted := true
	for i := range jumps[:len(jumps)-1] {
		lt, gt := less(i, i+1), less(i+1, i)
		if !(lt || !gt) { // not less than or equal
			sorted = false
			break
		}
	}
	if !sorted {
		jumps = append([]Jump{}, jumps...)
		sort.Slice(jumps, less)
	}
	return jumps
}

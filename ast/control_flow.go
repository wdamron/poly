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
	"github.com/wdamron/poly/types"
)

var _ Expr = (*ControlFlow)(nil)

var (
	ControlFlowEntryIndex  = -2
	ControlFlowReturnIndex = -1
)

// Block is an expression sequence within a control flow graph.
type Block struct {
	// Index of the block within the Blocks slice of a control flow graph
	Index int
	// The last expression in Sequence is the result type for the block
	Sequence []Expr
}

// Jump is a "goto" between a pair of blocks in a control flow graph.
type Jump struct {
	// From and To are block indexes
	From, To int
}

// Check if j is a jump from the entry block.
func (j *Jump) FromEntry() bool { return j.From == ControlFlowEntryIndex }

// Check if j is a jump to the return block.
func (j *Jump) ToReturn() bool { return j.To == ControlFlowReturnIndex }

// Control flow graph
type ControlFlow struct {
	Name string
	// Locals are shared variable names used within the control flow expression,
	// bound as mutable reference types during inference
	Locals []string
	Entry  Block
	// Return determines the result type of a control flow expression
	Return Block
	Blocks []Block
	// Jumps do not affect inferred types
	Jumps    []Jump
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

// Assign an expression sequence to the entry block for e.
func (e *ControlFlow) SetEntry(block ...Expr) {
	e.Entry.Sequence = block
}

// Assign an expression sequence to the return block for e. The return block determines
// the result type of a control flow expression.
func (e *ControlFlow) SetReturn(block ...Expr) {
	e.Return.Sequence = block
}

// Add a new block to e. Each block should contain a sequence of 1 or more expressions.
func (e *ControlFlow) AddBlock(block ...Expr) Block {
	e.Blocks = append(e.Blocks, Block{len(e.Blocks), block})
	return e.Blocks[len(e.Blocks)-1]
}

// Add a jump for a pair of blocks. Jumps do not affect inferred types. The return block
// determines the result type of a control flow expression.
func (e *ControlFlow) AddJump(from, to Block) {
	if !e.HasJump(from, to) {
		e.Jumps = append(e.Jumps, Jump{from.Index, to.Index})
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

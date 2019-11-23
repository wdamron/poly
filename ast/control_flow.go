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

// Control flow graph
type ControlFlow struct {
	// Local (shared) variable names
	Locals   []string
	Entry    Block
	Return   Block
	Blocks   []Block
	Jumps    []Jump
	inferred types.Type
}

type Block struct {
	Index    int
	Sequence []Expr
}

type Jump struct {
	From, To int
}

// "ControlFlow"
func (e *ControlFlow) ExprName() string { return "ControlFlow" }

// Get the inferred (or assigned) type of e.
func (e *ControlFlow) Type() types.Type { return types.RealType(e.inferred) }

// Assign a type to e. Type assignments should occur indirectly, during inference.
func (e *ControlFlow) SetType(t types.Type) { e.inferred = t }

func (e *ControlFlow) SetEntry(block ...Expr) {
	e.Entry = Block{-1, block}
}

func (e *ControlFlow) SetReturn(block ...Expr) {
	e.Return = Block{-2, block}
}

func (e *ControlFlow) AddBlock(block ...Expr) Block {
	e.Blocks = append(e.Blocks, Block{len(e.Blocks), block})
	return e.Blocks[len(e.Blocks)-1]
}

func (e *ControlFlow) AddJump(from, to Block) {
	if !e.HasJump(from, to) {
		e.Jumps = append(e.Jumps, Jump{from.Index, to.Index})
	}
}

func (e *ControlFlow) HasJump(from, to Block) bool {
	for _, j := range e.Jumps {
		if j.From == from.Index && j.To == to.Index {
			return true
		}
	}
	return false
}

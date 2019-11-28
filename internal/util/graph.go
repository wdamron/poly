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

package util

type Graph [][]int

func NewGraph(numVerts int) Graph { return Graph(make([][]int, numVerts)) }

func (g Graph) AddEdge(from, to int) {
	if !g.HasEdge(from, to) {
		g[from] = append(g[from], to)
	}
}

func (g Graph) HasEdge(from, to int) bool {
	for _, succ := range g[from] {
		if succ == to {
			return true
		}
	}
	return false
}

func (g Graph) SCC() [][]int {
	state := sccState{
		indexTable: make([]int, len(g)),
		lowLink:    make([]int, len(g)),
		onStack:    make([]bool, len(g)),
	}
	for v := range g {
		if state.indexTable[v] == 0 {
			g.tarjanSCC(&state, v)
		}
	}
	sccs := state.sccs
	// Reverse the slice for topological ordering:
	for i, j := 0, len(sccs)-1; i < j; i, j = i+1, j-1 {
		sccs[i], sccs[j] = sccs[j], sccs[i]
	}
	return sccs
}

type sccState struct {
	index      int
	indexTable []int
	lowLink    []int
	onStack    []bool

	stack []int
	sccs  [][]int
}

// Tarjan's SCC algorithm, based on https://github.com/gonum/gonum/blob/master/graph/topo/tarjan.go
//
// Components will be output in reversed dependency-order. Reversing the output creates a proper topological sort.
func (g Graph) tarjanSCC(state *sccState, v int) {
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
	for _, succ := range g[v] {
		if state.indexTable[succ] == 0 {
			// Successor has not yet been visited; recur on it
			g.tarjanSCC(state, succ)
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

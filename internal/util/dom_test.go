// The MIT License (MIT)
//
// Copyright (c) 2019 West Damron
// Portions Copyright (c) 2017 Julian Jensen jjdanois@gmail.com
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

package util_test

import (
	"testing"

	. "github.com/wdamron/poly/internal/util"
)

func checkDoms(t *testing.T, expected, actual []int) {
	for i := range expected {
		if expected[i] != actual[i] {
			t.Fatalf("unexpected dominators: %#+v", actual)
		}
	}
}

func checkGraphs(t *testing.T, expected, actual [][]int) {
	for i := range expected {
		if len(actual[i]) != len(expected[i]) {
			t.Logf("expect:  %#+v", expected)
			t.Logf("actual: %#+v", actual)
			t.Fail()
			break
		}
		for j := range expected[i] {
			if actual[i][j] != expected[i][j] {
				t.Logf("expect:  %#+v", expected)
				t.Logf("actual: %#+v", actual)
				t.Fail()
				break
			}
		}
	}
}

func checkTree(t *testing.T, tree DomTree, expectDoms []int) {
	for dominee, idom := range expectDoms {
		if tree.Idom(dominee) != expectDoms[dominee] {
			t.Logf("tree: %#+v", tree)
			t.Fail()
			break
		}
		if !tree.Dominates(idom, dominee) {
			t.Logf("bad dominates method")
			t.Fail()
			break
		}

		found := false
		tree.Dominees(idom, func(id int) {
			if id == dominee {
				found = true
			}
			if !tree.Dominates(idom, id) {
				t.Logf("bad dominates method")
				t.Fail()
			}
		})
		if !found {
			t.Logf("bad dominates method")
			t.Fail()
			break
		}
	}
}

func TestDominators(t *testing.T) {
	const (
		entry = iota
		ret
		merge1
		merge2
		loop1Head
		loop1Body
		loop2Head
		loop2Body
		side1
		side2
	)

	g := Graph{
		entry:     {loop1Head, loop2Head, side1},
		loop1Head: {loop1Body},
		loop1Body: {loop1Head, side2},
		loop2Head: {loop2Body},
		loop2Body: {loop2Head, side1},
		side1:     {merge1, ret},
		side2:     {merge2, ret},
		merge1:    {ret},
		merge2:    {ret},
		ret:       {},
	}

	doms := g.ImmediateDominators(0)

	expect := []int{
		entry:     entry,
		loop1Head: entry,
		loop1Body: loop1Head,
		loop2Head: entry,
		loop2Body: loop2Head,
		side1:     entry,
		side2:     loop1Body,
		merge1:    side1,
		merge2:    side2,
		ret:       entry,
	}

	checkDoms(t, expect, doms)
}

func TestDominanceFrontiers(t *testing.T) {
	// http://pages.cs.wisc.edu/~fischer/cs701.f05/lectures/Lecture22.pdf
	const (
		A = iota
		B
		C
		D
		E
		F
	)

	g := Graph{
		A: {B, F},
		B: {C, D},
		C: {E},
		D: {E},
		E: {F},
		F: {},
	}

	expectDoms := []int{A: A, B: A, C: B, D: B, E: B, F: A}
	expectFrontiers := [][]int{A: {}, B: {F}, C: {E}, D: {E}, E: {F}, F: {}}

	doms, frontiers := g.DominanceFrontiers(0)

	checkDoms(t, expectDoms, doms)
	checkGraphs(t, expectFrontiers, frontiers)
}

func TestDominanceMisc(t *testing.T) {
	// +---[0]
	// |    |
	// |    v
	// | +-[1]
	// | |  |
	// | |  v
	// | | [2]<--+
	// | |  |    |
	// | +--v    |
	// |    |    |
	// | +-[3]-+ |
	// | |     | |
	// | v     v |
	// |[4]   [5]|
	// | |     | |
	// |  \   /  |
	// |   [6]---+
	// |    |
	// |    v
	// |   [7]
	// |    |
	// |    v
	// +-->[8]
	g := Graph{
		0: {1, 8},
		1: {2, 3},
		2: {3},
		3: {4, 5},
		4: {6},
		5: {6},
		6: {7, 2},
		7: {8},
		8: {},
	}

	expectDoms := []int{0, 0, 1, 1, 3, 3, 3, 6, 0}
	expectFrontiers := [][]int{{}, {8}, {3}, {2, 8}, {6}, {6}, {2, 8}, {8}, {}}
	expectControlDeps := [][]int{
		0: {1, 3, 6, 7},
		1: {2},
		3: {4, 5},
		6: {2, 3, 6},
		8: nil,
	}

	frontiers, tree := g.AnalyzeDominators(0)
	_, _, controlDeps := g.ControlDependencies(8)

	checkTree(t, tree, expectDoms)
	checkGraphs(t, expectFrontiers, frontiers)
	checkGraphs(t, expectControlDeps, controlDeps)

	g = Graph{
		0:  {1, 2, 3},
		1:  {4},
		2:  {1, 4, 5},
		3:  {6, 7},
		4:  {12},
		5:  {8},
		6:  {9},
		7:  {9, 10},
		8:  {5, 11},
		9:  {11},
		10: {9},
		11: {0, 9},
		12: {8},
	}

	expectDoms = []int{0, 0, 0, 0, 0, 0, 3, 3, 0, 0, 7, 0, 4}

	tree = g.DominatorTree(0)

	checkTree(t, tree, expectDoms)
}

func TestControlDependencies(t *testing.T) {
	// An Efficient Method of Computing Static Single Assignment Form (Cytron et al): https://c9x.me/compile/bib/ssa.pdf
	// Section 5: Construction of Control Dependencies

	// Figure 1:
	g := Graph{
		0:  {1, 13},
		1:  {2},
		2:  {3, 7},
		3:  {4, 5},
		4:  {6},
		5:  {6},
		6:  {8},
		7:  {8},
		8:  {9},
		9:  {10, 11},
		10: {11},
		11: {9, 12},
		12: {2, 13},
		13: {},
	}

	// Figure 7:
	expectControlDeps := [][]int{
		0:  {1, 2, 8, 9, 11, 12},
		2:  {3, 6, 7},
		3:  {4, 5},
		9:  {10},
		11: {9, 11},
		12: {2, 8, 9, 11, 12},
		13: nil,
	}

	_, _, controlDeps := g.ControlDependencies(13)

	checkGraphs(t, expectControlDeps, controlDeps)

	// http://pages.cs.wisc.edu/~fischer/cs701.f05/lectures/Lecture22.pdf
	const (
		A = iota
		B
		C
		D
		E
		F
		G
		H
	)

	g = Graph{
		A: {B, H},
		B: {C, G},
		C: {D, E},
		D: {F},
		E: {F},
		F: {B},
		G: {H},
		H: {},
	}

	_, _, controlDeps = g.ControlDependencies(H)

	expectControlDeps = [][]int{
		A: {B, G},
		B: {B, C, F},
		C: {D, E},
		D: {}, E: {}, F: {}, G: {}, H: {},
	}

	checkGraphs(t, expectControlDeps, controlDeps)
}

func TestPostdominators(t *testing.T) {
	// http://pages.cs.wisc.edu/~fischer/cs701.f05/lectures/Lecture22.pdf
	const (
		A = iota
		B
		C
		D
		E
		F
	)

	g := Graph{
		A: {B, F},
		B: {C, D},
		C: {E},
		D: {E},
		E: {F},
		F: {},
	}

	ipostdoms, frontiers, _ := g.ControlDependencies(5)

	expectPostdoms := []int{A: F, B: E, C: E, D: E, E: F, F: F}
	expectFrontiers := [][]int{A: {}, B: {A}, C: {B}, D: {B}, E: {A}, F: {}}

	checkDoms(t, expectPostdoms, ipostdoms)
	checkGraphs(t, expectFrontiers, frontiers)
}

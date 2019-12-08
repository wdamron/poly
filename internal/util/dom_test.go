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

package util

import (
	"testing"
)

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

	for i := range doms {
		if doms[i] != expect[i] {
			t.Fatalf("unexpected dominators: %#+v", doms)
		}
	}
}

func TestDominanceFrontiers(t *testing.T) {
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

	doms, frontiers := g.DominanceFrontiers(0)

	expect := []int{
		A: A,
		B: A,
		C: B,
		D: B,
		E: B,
		F: A,
	}

	for i := range doms {
		if doms[i] != expect[i] {
			t.Fatalf("unexpected dominators: %#+v", doms)
		}
	}

	if len(frontiers[A]) != 0 {
		t.Fatalf("frontier: %#+v", frontiers[A])
	}
	if len(frontiers[B]) != 1 || frontiers[B][0] != F {
		t.Fatalf("frontier: %#+v", frontiers[B])
	}
	if len(frontiers[C]) != 1 || frontiers[C][0] != E {
		t.Fatalf("frontier: %#+v", frontiers[C])
	}
	if len(frontiers[D]) != 1 || frontiers[D][0] != E {
		t.Fatalf("frontier: %#+v", frontiers[D])
	}
	if len(frontiers[E]) != 1 || frontiers[E][0] != F {
		t.Fatalf("frontier: %#+v", frontiers[E])
	}
	if len(frontiers[F]) != 0 {
		t.Fatalf("frontier: %#+v", frontiers[F])
	}
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
	expectControlDeps := [][]int{{1, 2, 3, 4, 5, 6, 7}, {2, 3, 6}, nil, nil, nil, nil, nil, nil, nil}

	frontiers, tree := g.AnalyzeDominators(0)
	_, _, controlDeps := g.ControlDependencies(len(g) - 1)

	for dominee, idom := range expectDoms {
		if tree.Idom(dominee) != expectDoms[dominee] {
			t.Logf("tree: %#+v", tree)
			t.Fail()
			break
		}
		if !tree.Dominates(idom, dominee) {
			t.Fatalf("bad dominates method")
		}

		found := false
		tree.Dominees(idom, func(id int) {
			if id == dominee {
				found = true
			}
			if !tree.Dominates(idom, id) {
				t.Fatalf("bad dominates method")
			}
		})
		if !found {
			t.Fatalf("bad dominees method")
		}
	}

	for i := range expectFrontiers {
		if len(frontiers[i]) != len(expectFrontiers[i]) {
			t.Logf("frontiers: %#+v", frontiers)
			t.Logf("exp:       %#+v", expectFrontiers)
			t.Fail()
			break
		}
		// frontiers are sorted:
		for j := range expectFrontiers[i] {
			if frontiers[i][j] != expectFrontiers[i][j] {
				t.Logf("frontiers: %#+v", frontiers)
				t.Logf("exp:       %#+v", expectFrontiers)
				t.Fail()
				break
			}
		}
	}

	for i := range expectControlDeps {
		if len(controlDeps[i]) != len(expectControlDeps[i]) {
			t.Logf("ctrl: %#+v", controlDeps)
			t.Logf("exp:  %#+v", expectControlDeps)
			t.Fail()
			break
		}
		// frontiers are sorted:
		for j := range expectControlDeps[i] {
			if controlDeps[i][j] != expectControlDeps[i][j] {
				t.Logf("ctrl: %#+v", controlDeps)
				t.Logf("exp:  %#+v", expectControlDeps)
				t.Fail()
				break
			}
		}
	}

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

	for id := range expectDoms {
		if tree.Idom(id) != expectDoms[id] {
			t.Logf("tree: %#+v", tree)
			t.Fail()
			break
		}
	}

	for dominee, idom := range expectDoms {

		if !tree.Dominates(idom, dominee) {
			t.Fatalf("bad dominates method")
		}

		found := false
		tree.Dominees(idom, func(id int) {
			if id == dominee {
				found = true
			}
			if !tree.Dominates(idom, id) {
				t.Fatalf("bad dominates method")
			}
		})
		if !found {
			t.Fatalf("bad dominees method")
		}
	}
}

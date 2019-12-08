// The MIT License (MIT)
//
// Copyright (c) 2019 West Damron
// Portions Copyright (c) 2017 Julian Jensen jjdanois@gmail.com
// Portions Copyright (c) 2013 The Go Authors
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
	"sort"
)

func (g Graph) Transpose() Graph {
	t := make(Graph, len(g))
	for pred, succs := range g {
		for _, succ := range succs {
			t[succ] = append(t[succ], pred)
		}
	}
	return t
}

func (g Graph) Compact() {
	for id, dupes := range g {
		switch len(dupes) {
		case 0, 1:
			continue
		case 2:
			if dupes[0] > dupes[1] {
				dupes[0], dupes[1] = dupes[1], dupes[0]
			}
			continue
		}
		sort.Ints(dupes)
		lastId, flat := -1, dupes[:0]
		for _, id := range dupes {
			if lastId != id {
				flat = append(flat, id)
			}
		}
		g[id] = flat[:len(flat):len(flat)]
	}
}

func (g Graph) PostOrder(entry int, reverse bool) []int {
	if len(g) == 0 {
		return nil
	}
	if len(g) == 1 {
		return []int{0}
	}
	order := make([]int, len(g))
	if len(g) <= 64 {
		g.postOrderSmall(entry, order, 0, 0)
	} else {
		g.postOrderLarge(entry, order, make([]bool, len(g)), 0)
	}
	if reverse {
		for i, j := 0, len(order)-1; i < j; i, j = i+1, j-1 {
			order[i], order[j] = order[j], order[i]
		}
	}
	return order
}

func (g Graph) postOrderLarge(curr int, order []int, seen []bool, i int) int {
	if seen[curr] {
		return i
	}
	seen[curr] = true
	for _, succ := range g[curr] {
		if !seen[succ] {
			seen[succ] = true
			i = g.postOrderLarge(succ, order, seen, i)
		}
	}
	order[i] = curr
	return i + 1
}

func (g Graph) postOrderSmall(curr int, order []int, seen uint64, i int) (uint64, int) {
	if seen&(1<<uint8(curr)) != 0 {
		return seen, i
	}
	seen |= 1 << uint8(curr)
	for _, succ := range g[curr] {
		seen, i = g.postOrderSmall(succ, order, seen, i)
	}
	order[i] = curr
	return seen, i + 1
}

func (g Graph) ImmediateDominators(entry int) []int {
	return g.TransposedImmediateDominators(g.Transpose(), entry)
}

// A Simple, Fast Dominance Algorithm: https://www.cs.rice.edu/~keith/Embed/dom.pdf
func (g Graph) TransposedImmediateDominators(transposed Graph, entry int) []int {
	type info struct {
		id    int
		post  int
		preds []int
		succs []int
	}
	post, infos, idoms := g.PostOrder(entry, false), make([]info, len(g)), make([]int, len(g))
	for pred, succs := range g {
		infos[pred] = info{id: pred, post: -1, preds: transposed[pred], succs: succs}
	}
	for id := range idoms {
		idoms[id] = -1
	}
	for order, id := range post {
		// All nodes must be reachable:
		if id == -1 {
			return nil
		}
		infos[id].post = order
	}
	// The 0th node is assumed to be the entry node:
	idoms[entry] = entry
	changed := true
	for changed {
		changed = false
		// Reverse preorder:
		for i := len(post) - 1; i >= 0; i-- {
			// Find dominators:
			id := post[i]
			if id == entry {
				continue // skip the entry node
			}
			info, idom := infos[id], -1
			for _, pred := range info.preds {
				if idoms[pred] == -1 {
					continue
				}
				if idom == -1 {
					idom = pred
					continue
				}
				finger1, finger2 := infos[pred], infos[idom]
				for finger1.post != finger2.post {
					for finger1.post < finger2.post {
						finger1 = infos[idoms[finger1.id]] // finger1 = idom(finger1)
					}
					for finger2.post < finger1.post {
						finger2 = infos[idoms[finger2.id]] // finger2 = idom(finger2)
					}
				}
				idom = finger1.id
			}
			if idoms[id] != idom {
				idoms[id] = idom
				changed = true
			}

		}
	}
	return idoms
}

func (g Graph) DominanceFrontiers(entry int) (idoms []int, frontiers [][]int) {
	return g.TransposedDominanceFrontiers(g.Transpose(), entry)
}

func (g Graph) TransposedDominanceFrontiers(transposed Graph, entry int) (idoms []int, frontiers [][]int) {
	idoms = g.TransposedImmediateDominators(transposed, entry)
	frontiers = transposed.DominanceFrontiersFromIdoms(idoms)
	return
}

func (transposed Graph) DominanceFrontiersFromIdoms(idoms []int) [][]int {
	if len(transposed) == 0 {
		return nil
	}
	if len(transposed) == 1 {
		return [][]int{{}}
	}
	frontiers := make([][]int, len(transposed))
	for succ, preds := range transposed {
		if len(preds) < 2 {
			continue
		}
		for _, runner := range preds {
			for runner != idoms[succ] {
				frontiers[runner] = append(frontiers[runner], succ)
				runner = idoms[runner]
			}
		}
	}
	Graph(frontiers).Compact()
	return frontiers
}

func (g Graph) AnalyzeDominators(entry int) (frontiers [][]int, tree DomTree) {
	return g.TransposedAnalyzeDominators(g.Transpose(), entry)
}

func (g Graph) TransposedAnalyzeDominators(transposed Graph, entry int) (frontiers [][]int, tree DomTree) {
	idoms := g.TransposedImmediateDominators(transposed, entry)
	frontiers = transposed.DominanceFrontiersFromIdoms(idoms)
	tree = g.DominatorTreeFromIdoms(idoms, entry)
	return
}

// An Efficient Method of Computing Static Single Assignment Form (Cytron et al): https://c9x.me/compile/bib/ssa.pdf
// Section 5: Construction of Control Dependencies
//
// Control dependences are essentially the dominance frontiers in the reverse graph of the control flow
// graph. Let X and Y be nodes in CFG. If X appears on every path from Y to Exit, then X postdominates Y.
// Like the dominator relation, the postdominator relation is reflexive and transitive. If X postdominates
// Y but X != Y, then X strictly postdominates Y. The immediate postdominator of Y is the closest strict
// postdominator of Y on any path from Y to Exit. In a postdominator tree, the children of a node X are all
// immediately postdominated by X.
//
// A CFG node Y is control dependent on a CFG node X if both of the following hold:
//   1. There is a path p : X ~> Y such that Y postdominates every node after X on p.
//   2. The node Y does not strictly postdominate the node X.
//
// The reverse control flow graph RCFG has the same nodes as the given control flow graph CFG, but has
// an edge [Y -> X] for each edge [X -> Y] in CFG. The roles of Entry and Exit are also reversed.
//
// The postdominator relation on CFG is the dominator relation on RCFG. Let X and Y be nodes in CFG.
// Then Y is control dependent on X in CFG if and only if X ∈ DF(Y) in RCFG.
//
// Algorithm for computing the set CD(X) of nodes control dependent on X:
//   1. build RCFG
//   2. build dominator tree for RCFG
//   3. build dominance frontier RDF for RCFG
//   4. for each node X do CD(X) <- ∅ end
//   5. for each node Y do
//   6.   for each X ∈ RDF(Y) do
//   7.     CD(X) <- CD(X) ∪ {Y}
//   8.   end
//   9. end
func (g Graph) ControlDependencies(exit int) (ipostdoms []int, frontiers [][]int, deps [][]int) {
	rcfg := g.Transpose()
	ipostdoms = rcfg.TransposedImmediateDominators(g, exit)
	frontiers = g.DominanceFrontiersFromIdoms(ipostdoms)
	deps = rcfg // reuse the temporary RCFG
	for i := range deps {
		deps[i] = nil
	}
	for y := range g {
		for x := range frontiers[y] {
			deps[x] = append(deps[x], y)
		}
	}
	return
}

func (g Graph) Postdominators(exit int) []int {
	return g.Transpose().TransposedImmediateDominators(g, exit)
}

func (g Graph) PostdominanceFrontiers(exit int) (ipostdoms []int, frontiers [][]int) {
	ipostdoms = g.Transpose().TransposedImmediateDominators(g, exit)
	frontiers = g.DominanceFrontiersFromIdoms(ipostdoms)
	return
}

func (g Graph) AnalyzePostdominators(exit int) (frontiers [][]int, tree DomTree) {
	return g.Transpose().TransposedAnalyzeDominators(g, exit)
}

func (g Graph) TransposedAnalyzePostdominators(transposed Graph, exit int) (frontiers [][]int, tree DomTree) {
	return transposed.TransposedAnalyzeDominators(g, exit)
}

type DomTree struct {
	verts []domInfo
	edges [][]int
}

type domInfo struct {
	id, idom, pre, post int
}

func (g Graph) DominatorTree(entry int) DomTree {
	return g.DominatorTreeFromIdoms(g.TransposedImmediateDominators(g.Transpose(), entry), entry)
}

func (g Graph) PostdominatorTree(exit int) DomTree {
	rcfg := g.Transpose()
	return rcfg.DominatorTreeFromIdoms(rcfg.TransposedImmediateDominators(g, exit), exit)
}

func (g Graph) DominatorTreeFromIdoms(idoms []int, entry int) DomTree {
	t := DomTree{
		verts: make([]domInfo, len(g)),
		edges: make([][]int, len(g)),
	}
	for pred := range g {
		t.verts[pred] = domInfo{id: pred, idom: idoms[pred]}
	}
	for dominee, idom := range idoms {
		t.edges[idom] = append(t.edges[idom], dominee)
	}
	if len(g) <= 64 {
		t.numberSmall(entry, 0, 0, 0)
	} else {
		t.numberLarge(entry, 0, 0, make([]bool, len(g)))
	}
	return t
}

func (t DomTree) Len() int { return len(t.verts) }

func (t DomTree) ForEach(walk func(int)) {
	for id := range t.verts {
		walk(id)
	}
}

func (t DomTree) Dominees(id int, walk func(int)) {
	for _, child := range t.edges[id] {
		walk(child)
	}
}

func (t DomTree) Idom(id int) int { return t.verts[id].idom }

func (t DomTree) Dominates(a, b int) bool {
	ad, bd := t.verts[a], t.verts[b]
	return ad.pre <= bd.pre && bd.post <= ad.post
}

func (t *DomTree) numberLarge(id, pre, post int, seen []bool) (int, int, []bool) {
	if seen[id] {
		return pre, post, seen
	}
	seen[id] = true
	t.verts[id].pre = pre
	pre++
	for _, child := range t.edges[id] {
		pre, post, seen = t.numberLarge(child, pre, post, seen)
	}
	t.verts[id].post = post
	post++
	return pre, post, seen
}

func (t *DomTree) numberSmall(id, pre, post int, seen uint64) (int, int, uint64) {
	if seen&(1<<uint8(id)) != 0 {
		return pre, post, seen
	}
	seen |= 1 << uint8(id)
	t.verts[id].pre = pre
	pre++
	for _, child := range t.edges[id] {
		pre, post, seen = t.numberSmall(child, pre, post, seen)
	}
	t.verts[id].post = post
	post++
	return pre, post, seen
}

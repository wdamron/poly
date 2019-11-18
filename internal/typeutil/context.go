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

package typeutil

import (
	"github.com/wdamron/poly/types"
)

type StashedType struct {
	Name string
	Type types.Type
}

type StashedLink struct {
	v    *types.Var
	prev types.Var
}

func (l *StashedLink) Restore() { *l.v = l.prev }

type CommonContext struct {
	VarTracker    VarTracker
	EnvStash      []StashedType            // shadowed variables
	LinkStash     []StashedLink            // stashed type-variables (during speculative unification)
	InstLookup    map[int]*types.Var       // instantiation lookup for generic type-variables
	RecInstLookup map[int]*types.Recursive // instantiation lookup for mutually-recursive type groups
	Speculate     bool

	// cache of the most-recently matched instance by type-class id:
	LastInstanceMatch map[int]*types.Instance

	// initial space:
	_envStash  [32]StashedType
	_linkStash [32]StashedLink
}

func (ctx *CommonContext) Init() {
	ctx.EnvStash, ctx.LinkStash, ctx.InstLookup, ctx.RecInstLookup =
		ctx._envStash[:0], ctx._linkStash[:0], make(map[int]*types.Var, 16), make(map[int]*types.Recursive, 16)
}

func (ctx *CommonContext) Reset() {
	ctx.VarTracker.Reset()
	for i := range ctx._envStash {
		ctx._envStash[i] = StashedType{}
	}
	ctx.EnvStash, ctx.LinkStash = ctx._envStash[:0], ctx._linkStash[:0]
	ctx.ClearInstantiationLookup()
	ctx.ClearRecursiveInstantiationLookup()
	ctx.ClearLastInstanceCache()
}

func (ctx *CommonContext) ClearInstantiationLookup() {
	for k := range ctx.InstLookup {
		delete(ctx.InstLookup, k)
	}
}

func (ctx *CommonContext) ClearRecursiveInstantiationLookup() {
	for k := range ctx.RecInstLookup {
		delete(ctx.RecInstLookup, k)
	}
}

func (ctx *CommonContext) ClearLastInstanceCache() {
	for k := range ctx.LastInstanceMatch {
		delete(ctx.LastInstanceMatch, k)
	}
}

// returns 1 if the variable was stashed, otherwise 0
func (ctx *CommonContext) Stash(env types.TypeEnv, name string) int {
	if existing := env.Lookup(name); existing != nil {
		ctx.EnvStash = append(ctx.EnvStash, StashedType{name, existing})
		return 1
	}
	return 0
}

func (ctx *CommonContext) Unstash(env types.TypeEnv, count int) {
	if count <= 0 {
		return
	}
	stash := ctx.EnvStash
	unstashed := 0
	for i := len(stash) - 1; unstashed < count && i >= 0; i, unstashed = i-1, unstashed+1 {
		env.Assign(stash[i].Name, stash[i].Type)
	}
	ctx.EnvStash = ctx.EnvStash[0 : len(stash)-unstashed]
}

func (ctx *CommonContext) StashLink(v *types.Var) {
	ctx.LinkStash = append(ctx.LinkStash, StashedLink{v, *v})
}

func (ctx *CommonContext) UnstashLinks(count int) {
	if count <= 0 {
		return
	}
	stash := ctx.LinkStash
	for i := len(stash) - 1; i > len(stash)-1-count; i-- {
		stash[i].Restore()
	}
}

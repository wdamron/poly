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
	"github.com/wdamron/poly/ast"
	"github.com/wdamron/poly/types"
)

// Shadowed variables
type StashedType struct {
	Name string
	Type types.Type
}

// Stashed type-variables (during speculative unification)
type StashedLink struct {
	v    *types.Var
	prev types.Var
}

func (l *StashedLink) Restore() { *l.v = l.prev }

// Used for deferred instance matching (when multiple instances match)
type DeferredConstraint struct {
	Var  *types.Var
	Expr ast.Expr
}

type CommonContext struct {
	VarTracker          VarTracker              // type-variables generated during inference
	EnvStash            []StashedType           // shadowed variables
	LinkStash           []StashedLink           // stashed type-variables (during speculative unification)
	InstLookup          map[uint]*types.Var     // instantiation lookup for generic type-variables
	VarScopes           map[string][]*ast.Scope // map from variable name to defining scope and shadowed scopes (stacked)
	ScopeStack          []ast.Scope             // stack of nested binding scopes during inference
	DeferredConstraints []DeferredConstraint    // deferred instance matching (when multiple instances match)
	CurrentExpr         ast.Expr                // added to deferred constraints during unification for debugging

	// modes:
	Speculate                   bool // stash linked type-variables during unification
	TrackScopes                 bool // track defining scopes for variables during inference
	DeferredConstraintsEnabled  bool // allow deferred unification when multiple instances match
	CheckingDeferredConstraints bool // prevent additional deferred constraints

	// initial space:
	_envStash            [32]StashedType
	_linkStash           [32]StashedLink
	_deferredConstraints [64]DeferredConstraint
}

func (ctx *CommonContext) Init() {
	if ctx.InstLookup != nil {
		return
	}
	ctx.EnvStash, ctx.LinkStash = ctx._envStash[:0], ctx._linkStash[:0]
	ctx.InstLookup = make(map[uint]*types.Var, 16)
	ctx.DeferredConstraints = ctx._deferredConstraints[:0]
	ctx.VarScopes = make(map[string][]*ast.Scope)
}

func (ctx *CommonContext) Reset() {
	ctx.VarTracker.Reset()
	ctx.TrackScopes, ctx.DeferredConstraintsEnabled = false, false
	for i := range ctx._envStash {
		ctx._envStash[i] = StashedType{}
	}
	for i := range ctx._envStash {
		ctx._envStash[i] = StashedType{}
	}
	for i := range ctx._linkStash {
		ctx._linkStash[i] = StashedLink{}
	}
	for i := range ctx.DeferredConstraints {
		ctx.DeferredConstraints[i] = DeferredConstraint{}
	}
	ctx.EnvStash, ctx.LinkStash, ctx.DeferredConstraints = ctx._envStash[:0], ctx._linkStash[:0], ctx._deferredConstraints[:0]
	ctx.ClearInstantiationLookup()
	ctx.ResetScopeStack()
}

func (ctx *CommonContext) ClearInstantiationLookup() {
	for k := range ctx.InstLookup {
		delete(ctx.InstLookup, k)
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

func (ctx *CommonContext) ResetScopeStack() {
	ctx.ScopeStack = nil
}

func (ctx *CommonContext) EnterScope(expr ast.Expr) {
	if !ctx.TrackScopes {
		return
	}
	var parent *ast.Scope
	if len(ctx.ScopeStack) != 0 {
		parent = &ctx.ScopeStack[len(ctx.ScopeStack)-1]
	}
	ctx.ScopeStack = append(ctx.ScopeStack, ast.Scope{expr, parent})
}

func (ctx *CommonContext) LeaveScope() {
	if ctx.TrackScopes {
		ctx.ScopeStack = ctx.ScopeStack[:len(ctx.ScopeStack)-1]
	}
}

func (ctx *CommonContext) PushVarScope(name string) {
	if ctx.TrackScopes {
		ctx.VarScopes[name] = append(ctx.VarScopes[name], &ctx.ScopeStack[len(ctx.ScopeStack)-1])
	}
}

func (ctx *CommonContext) PopVarScope(name string) {
	if !ctx.TrackScopes {
		return
	}
	scopes := ctx.VarScopes[name]
	if len(scopes) == 1 {
		delete(ctx.VarScopes, name)
	} else {
		ctx.VarScopes[name] = scopes[:len(scopes)-1]
	}
}

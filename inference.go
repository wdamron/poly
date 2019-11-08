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

// poly provides inference for a polymorphic type-system with extensible records and variants.
//
// The type-system is an extension of Hindley-Milner based on Daan Leijen's paper: Extensible Records with Scoped Labels (Microsoft Research).
//
// The implementation is based on an OCaml library by Tom Primozic.
//
// Links:
//
// * extensible_rows2 (OCaml implementation): https://github.com/tomprimozic/type-systems/tree/master/extensible_rows2
//
// * Extensible Records with Scoped Labels (Leijen, 2005): https://www.microsoft.com/en-us/research/publication/extensible-records-with-scoped-labels/
//
// * Efficient Generalization with Levels (Oleg Kiselyov): http://okmij.org/ftp/ML/generalization.html#levels
//
// * Hindley-Milner type system (Wikipedia): https://en.wikipedia.org/wiki/Hindley–Milner_type_system
package poly

import (
	"errors"

	"github.com/wdamron/poly/ast"
	"github.com/wdamron/poly/internal/typeutil"
	"github.com/wdamron/poly/types"
)

type stashedType struct {
	Name string
	Type types.Type
}

type stashedLink struct {
	v    *types.Var
	prev types.Var
}

func (l *stashedLink) restore() { *l.v = l.prev }

type commonContext struct {
	varTracker typeutil.VarTracker
	envStash   []stashedType      // shadowed variables
	linkStash  []stashedLink      // stashed type-variables (during speculative unification)
	instLookup map[int]*types.Var // instantiation lookup for generic type-variables
	speculate  bool

	// initial space:
	_envStash  [32]stashedType
	_linkStash [32]stashedLink
}

func (ctx *commonContext) init() {
	ctx.envStash, ctx.linkStash, ctx.instLookup = ctx._envStash[:0], ctx._linkStash[:0], make(map[int]*types.Var, 16)
}

func (ctx *commonContext) reset() {
	ctx.varTracker.Reset()
	for i := range ctx._envStash {
		ctx._envStash[i] = stashedType{}
	}
	ctx.envStash, ctx.linkStash = ctx._envStash[:0], ctx._linkStash[:0]
	ctx.clearInstantiationLookup()
}

func (ctx *commonContext) clearInstantiationLookup() {
	for k := range ctx.instLookup {
		delete(ctx.instLookup, k)
	}
}

// returns 1 if the variable was stashed, otherwise 0
func (ctx *commonContext) stash(env *TypeEnv, name string) int {
	if existing, exists := env.Types[name]; exists {
		ctx.envStash = append(ctx.envStash, stashedType{name, existing})
		return 1
	}
	return 0
}

func (ctx *commonContext) unstash(env *TypeEnv, count int) {
	if count <= 0 {
		return
	}
	stash := ctx.envStash
	unstashed := 0
	for i := len(stash) - 1; unstashed < count && i >= 0; i, unstashed = i-1, unstashed+1 {
		env.Types[stash[i].Name] = stash[i].Type
	}
	ctx.envStash = ctx.envStash[0 : len(stash)-unstashed]
}

func (ctx *commonContext) stashLink(v *types.Var) {
	ctx.linkStash = append(ctx.linkStash, stashedLink{v, *v})
}

func (ctx *commonContext) unstashLinks(count int) {
	if count <= 0 {
		return
	}
	stash := ctx.linkStash
	for i := len(stash) - 1; i > len(stash)-1-count; i-- {
		*stash[i].v = stash[i].prev
	}
}

// InferenceContext is a re-usable context for type inference.
//
// An inference context cannot be used concurrently.
type InferenceContext struct {
	common      commonContext
	err         error
	invalid     ast.Expr
	rootExpr    ast.Expr
	analysis    analysis
	initialized bool
	analyzed    bool
	needsReset  bool
	annotate    bool
}

// Create a new type-inference context. A context may be re-used across calls of ExprType.
func NewContext() *InferenceContext {
	ti := &InferenceContext{}
	ti.init()
	return ti
}

func (ti *InferenceContext) init() {
	ti.analysis.init()
	ti.common.init()
	ti.initialized = true
}

func (ti *InferenceContext) reset() {
	if ti.analyzed {
		ti.analysis.reset()
		ti.analyzed = false
	}
	ti.common.reset()
	ti.rootExpr, ti.err, ti.invalid, ti.needsReset =
		nil, nil, nil, false
}

// Reset the state of the context. The context will be reset automatically between calls of ExprType.
func (ti *InferenceContext) Reset() {
	if !ti.needsReset {
		return
	}
	ti.reset()
}

// Get the error which caused inference to fail.
func (ti *InferenceContext) Error() error { return ti.err }

// Get the expression which caused inference to fail.
func (ti *InferenceContext) InvalidExpr() ast.Expr { return ti.invalid }

// Infer the type of expr within env.
//
// A type-environment cannot be used concurrently for inference; to share a type-environment
// across threads, create a new type-environment for each thread which inherits from the
// shared environment.
func (ti *InferenceContext) Infer(expr ast.Expr, env *TypeEnv) (types.Type, error) {
	nocopy := false
	_, t, err := ti.inferRoot(expr, env, nocopy)
	return t, err
}

// Infer the type of expr within env. The type-annotated copy of expr will be returned.
//
// A type-environment cannot be used concurrently for inference; to share a type-environment
// across threads, create a new type-environment for each thread which inherits from the
// shared environment.
func (ti *InferenceContext) Annotate(expr ast.Expr, env *TypeEnv) (ast.Expr, error) {
	nocopy := false
	ti.annotate = true
	root, _, err := ti.inferRoot(expr, env, nocopy)
	ti.annotate = false
	return root, err
}

// Infer the type of expr within env. Type-annotations will be added directly to expr.
// All sub-expressions of expr must have unique addresses.
//
// A type-environment cannot be used concurrently for inference; to share a type-environment
// across threads, create a new type-environment for each thread which inherits from the
// shared environment.
func (ti *InferenceContext) AnnotateDirect(expr ast.Expr, env *TypeEnv) error {
	nocopy := true
	ti.annotate = true
	_, _, err := ti.inferRoot(expr, env, nocopy)
	ti.annotate = false
	return err
}

func (ti *InferenceContext) inferRoot(root ast.Expr, env *TypeEnv, nocopy bool) (ast.Expr, types.Type, error) {
	if root == nil {
		return nil, nil, errors.New("Empty expression")
	}
	if !nocopy {
		root = ast.CopyExpr(root)
	}
	env = NewTypeEnv(env)
	if ti.needsReset {
		ti.reset()
	} else if !ti.initialized {
		ti.init()
	}
	ti.rootExpr, ti.common.varTracker.NextId = root, env.NextVarId
	t, err := ti.infer(env, 0, root)
	ti.needsReset, ti.rootExpr, env.NextVarId = true, nil, ti.common.varTracker.NextId
	if err != nil {
		return root, t, err
	}
	t = generalize(-1, t)
	ti.common.varTracker.FlattenLinks()
	return root, t, nil
}

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

// InferenceContext is a re-usable context for type inference.
type InferenceContext struct {
	varTracker  typeutil.VarTracker
	envStash    []stashedType      // shadowed variables
	instLookup  map[int]*types.Var // instantiation lookup for generic type-variables
	err         error
	invalid     ast.Expr
	rootExpr    ast.Expr
	analysis    analysis
	initialized bool
	analyzed    bool
	needsReset  bool
	annotate    bool

	// initial space:
	_envStash [32]stashedType
}

type stashedType struct {
	Name string
	Type types.Type
}

// Create a new type-inference context. A context may be re-used across calls of ExprType.
func NewContext() *InferenceContext {
	ti := &InferenceContext{}
	ti.init()
	return ti
}

// Infer the type of expr within env.
func (ti *InferenceContext) Infer(expr ast.Expr, env *TypeEnv) (types.Type, error) {
	nocopy := false
	_, t, err := ti.inferRoot(expr, env, nocopy)
	return t, err
}

// Infer the type of expr within env. The type-annotated copy of expr will be returned.
func (ti *InferenceContext) Annotate(expr ast.Expr, env *TypeEnv) (ast.Expr, error) {
	nocopy := false
	ti.annotate = true
	root, _, err := ti.inferRoot(expr, env, nocopy)
	ti.annotate = false
	return root, err
}

// Infer the type of expr within env. Type-annotations will be added directly to expr.
// All sub-expressions of expr must have unique addresses.
func (ti *InferenceContext) AnnotateDirect(expr ast.Expr, env *TypeEnv) error {
	nocopy := true
	ti.annotate = true
	_, _, err := ti.inferRoot(expr, env, nocopy)
	ti.annotate = false
	return err
}

// Get the error which caused inference to fail.
func (ti *InferenceContext) Error() error { return ti.err }

// Get the expression which caused inference to fail.
func (ti *InferenceContext) InvalidExpr() ast.Expr { return ti.invalid }

// Reset the state of the context. The context will be reset automatically between calls of ExprType.
func (ti *InferenceContext) Reset() {
	if !ti.needsReset {
		return
	}
	ti.reset()
}

func (ti *InferenceContext) init() {
	ti.analysis.init()
	ti.envStash, ti.instLookup, ti.initialized =
		ti._envStash[:0], make(map[int]*types.Var, 16), true
}

func (ti *InferenceContext) reset() {
	ti.clearInstLookup()
	if ti.analyzed {
		ti.analysis.reset()
		ti.analyzed = false
	}
	for i := range ti._envStash {
		ti._envStash[i] = stashedType{}
	}
	ti.varTracker.Reset()
	ti.rootExpr, ti.err, ti.invalid, ti.envStash, ti.needsReset =
		nil, nil, nil, ti._envStash[:0], false
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
	ti.rootExpr, ti.varTracker.NextId = root, env.NextVarId
	t, err := ti.infer(env, 0, root)
	ti.needsReset, ti.rootExpr = true, nil
	if err != nil {
		return root, t, err
	}
	t = generalize(-1, t)
	ti.varTracker.FlattenLinks()
	return root, t, nil
}

func (ti *InferenceContext) clearInstLookup() {
	for id := range ti.instLookup {
		delete(ti.instLookup, id)
	}
}

// returns 1 if the variable was stashed, otherwise 0
func (ti *InferenceContext) stash(env *TypeEnv, name string) int {
	if existing, exists := env.Types[name]; exists {
		ti.envStash = append(ti.envStash, stashedType{name, existing})
		return 1
	}
	return 0
}

func (ti *InferenceContext) unstash(env *TypeEnv, count int) {
	if count <= 0 {
		return
	}
	stash := ti.envStash
	unstashed := 0
	for i := len(stash) - 1; unstashed < count && i >= 0; i, unstashed = i-1, unstashed+1 {
		env.Types[stash[i].Name] = stash[i].Type
	}
	ti.envStash = ti.envStash[0 : len(stash)-unstashed]
}

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
	"github.com/wdamron/poly/types"
)

// Inference is a re-usable context for type inference.
type Inference struct {
	nextId     int                // next unused type-variable id
	envStash   []stashedType      // shadowed variables
	instLookup map[int]*types.Var // instantiation lookup for generic type-variables
	err        error
	invalid    ast.Expr
	rootExpr   ast.Expr
	analysis   analysis
	analyzed   bool
	needsReset bool

	// initial space:
	_envStash [32]stashedType
}

type stashedType struct {
	Name string
	Type types.Type
}

// Create a new type-inference context. A context may be re-used across calls of ExprType.
func NewInference() *Inference {
	ti := &Inference{instLookup: make(map[int]*types.Var, 16)}
	ti.analysis.init()
	ti.envStash = ti._envStash[:0]
	return ti
}

// Infer the type of expr within env.
func (ti *Inference) ExprType(env *TypeEnv, expr ast.Expr) (types.Type, error) {
	if expr == nil {
		return nil, errors.New("Empty expression")
	}
	if env == nil {
		env = NewTypeEnv()
	}
	if ti.needsReset {
		ti.Reset()
	}
	ti.nextId = env.nextId
	ti.rootExpr = expr
	t, err := ti.infer(env.Map(), 0, expr)
	ti.needsReset, ti.rootExpr = true, nil
	if err != nil {
		return t, err
	}
	return generalize(-1, t), nil
}

// Return the error which caused inference to fail.
func (ti *Inference) Error() error { return ti.err }

// Return the expression which caused inference to fail.
func (ti *Inference) InvalidExpr() ast.Expr { return ti.invalid }

// Reset the state of the context. The context will be reset automatically between calls of ExprType.
func (ti *Inference) Reset() {
	if !ti.needsReset {
		return
	}
	ti.clearInstLookup()
	if ti.analyzed {
		ti.analysis.reset()
		ti.analyzed = false
	}
	for i := range ti._envStash {
		ti._envStash[i] = stashedType{}
	}
	ti.nextId, ti.rootExpr, ti.err, ti.invalid, ti.envStash, ti.needsReset =
		0, nil, nil, nil, ti._envStash[:0], false
}

func (ti *Inference) freshId() int {
	id := ti.nextId
	ti.nextId++
	return id
}

func (ti *Inference) newVar(level int) *types.Var {
	return types.NewVar(ti.freshId(), level)
}

func (ti *Inference) newVars(level, count int) []types.Var {
	vars := make([]types.Var, count)
	for i := range vars {
		vars[i].Update(ti.freshId(), level, nil)
	}
	return vars
}

func (ti *Inference) newGenVar() *types.Var {
	return types.NewGenericVar(ti.freshId())
}

// returns 1 if the variable was stashed, otherwise 0
func (ti *Inference) stash(env map[string]types.Type, name string) int {
	if existing, exists := env[name]; exists {
		ti.envStash = append(ti.envStash, stashedType{name, existing})
		return 1
	}
	return 0
}

func (ti *Inference) unstash(env map[string]types.Type, count int) {
	if count <= 0 {
		return
	}
	stash := ti.envStash
	unstashed := 0
	for i := len(stash) - 1; unstashed < count && i >= 0; i, unstashed = i-1, unstashed+1 {
		env[stash[i].Name] = stash[i].Type
	}
	ti.envStash = ti.envStash[0 : len(stash)-unstashed]
}

func (ti *Inference) clearInstLookup() {
	for id := range ti.instLookup {
		delete(ti.instLookup, id)
	}
}

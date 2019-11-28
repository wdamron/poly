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

package poly

import (
	"errors"

	"github.com/wdamron/poly/ast"
	"github.com/wdamron/poly/internal/astutil"
	"github.com/wdamron/poly/internal/typeutil"
	"github.com/wdamron/poly/types"
)

// InferenceContext is a re-usable context for type inference.
//
// An inference context cannot be used concurrently.
type InferenceContext struct {
	common        typeutil.CommonContext
	err           error
	invalid       ast.Expr
	rootExpr      ast.Expr
	analysis      astutil.Analysis
	letGroupCount int
	initialized   bool
	analyzed      bool
	needsReset    bool
	annotate      bool
}

// Create a new type-inference context. A context may be reused for inference.
func NewContext() *InferenceContext {
	ti := &InferenceContext{}
	ti.init()
	return ti
}

func (ti *InferenceContext) init() {
	ti.analysis.Init()
	ti.common.Init()
	ti.initialized = true
}

func (ti *InferenceContext) reset() {
	if ti.analyzed {
		ti.analysis.Reset()
		ti.analyzed = false
	}
	ti.common.Reset()
	ti.rootExpr, ti.err, ti.invalid, ti.letGroupCount, ti.needsReset =
		nil, nil, nil, 0, false
}

// Reset the state of the context. The context will be reset automatically before inference.
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
	nocopy := true
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
	ti.rootExpr, ti.common.VarTracker.NextId = root, env.NextVarId
	t, err := ti.infer(env, types.TopLevel+1, root)
	ti.needsReset, ti.rootExpr, env.NextVarId = true, nil, ti.common.VarTracker.NextId
	if err != nil {
		return root, t, err
	}
	t = Generalize(t)
	ti.common.VarTracker.FlattenLinks()
	return root, t, nil
}

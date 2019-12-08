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
	"github.com/wdamron/poly/types"
)

// InferenceContext is a reusable context for type inference.
//
// An inference context cannot be used concurrently.
type InferenceContext struct {
	annotate      bool
	canDeferMatch bool
	analyzed      bool
	needsReset    bool

	rootExpr      ast.Expr
	analysis      *astutil.Analysis
	letGroupCount int

	err     error
	invalid ast.Expr
}

// Create a new type-inference context. A context may be reused for inference.
func NewContext() *InferenceContext { return &InferenceContext{} }

func (ti *InferenceContext) reset() {
	if ti.analyzed {
		ti.analysis.Reset()
		ti.analyzed = false
	}
	ti.rootExpr, ti.err, ti.invalid, ti.letGroupCount, ti.needsReset = nil, nil, nil, 0, false
}

// Reset the state of the context. The context will be reset automatically before inference.
func (ti *InferenceContext) Reset() {
	if !ti.needsReset {
		return
	}
	ti.reset()
}

// Deferred instance-matching may find matching type-classes when the surrounding context
// does not initially contain enough information to determine a match. Deferred matching
// may (or may not) lead to unsound types for previously inferred expressions.
//
// By default, deferred instance-matching is disabled.
func (ti *InferenceContext) EnableDeferredInstanceMatching(enabled bool) { ti.canDeferMatch = enabled }

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
	if ti.needsReset {
		ti.reset()
	}
	ti.rootExpr, env.common.TrackScopes, env.common.DeferredConstraintsEnabled = root, ti.annotate, ti.canDeferMatch
	t, err := ti.infer(env, types.TopLevel+1, root)
	if err != nil {
		goto Cleanup
	}
	if invalid, err := env.common.ApplyDeferredConstraints(); err != nil {
		ti.invalid, ti.err = invalid, err
		goto Cleanup
	}
	env.common.VarTracker.FlattenLinks()
	t = Generalize(t)
Cleanup:
	env.common.Reset()
	ti.needsReset, ti.rootExpr = true, nil
	return root, t, ti.err
}

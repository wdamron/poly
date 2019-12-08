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
	"errors"

	"github.com/wdamron/poly/ast"
	"github.com/wdamron/poly/types"
)

// See "Efficient Generalization with Levels" (Oleg Kiselyov)
// http://okmij.org/ftp/ML/generalization.html#levels
//
// This implementation follows the sound_eager algorithm.
func (ctx *CommonContext) occursAdjustLevels(id, level uint, t types.Type) error {
	switch t := t.(type) {
	case *types.Var:
		switch {
		case t.IsLinkVar():
			return ctx.occursAdjustLevels(id, level, t.Link())
		case t.IsGenericVar():
			return errors.New("Types must be instantiated before checking for recursion")
		default: // weak or unbound
			if t.Id() == id {
				return errors.New("Implicitly recursive types are not supported")
			}
			if t.LevelNum() > level {
				if ctx.Speculate {
					ctx.StashLink(t)
				}
				t.SetLevelNum(level)
			}
		}
		return nil

	case *types.App:
		if err := ctx.occursAdjustLevels(id, level, t.Const); err != nil {
			return err
		}
		if t.Underlying != nil {
			if err := ctx.occursAdjustLevels(id, level, t.Underlying); err != nil {
				return err
			}
		}
		for _, param := range t.Params {
			if err := ctx.occursAdjustLevels(id, level, param); err != nil {
				return err
			}
		}
		return nil

	case *types.Arrow:
		for _, arg := range t.Args {
			if err := ctx.occursAdjustLevels(id, level, arg); err != nil {
				return err
			}
		}
		return ctx.occursAdjustLevels(id, level, t.Return)

	case *types.Record:
		return ctx.occursAdjustLevels(id, level, t.Row)

	case *types.Variant:
		return ctx.occursAdjustLevels(id, level, t.Row)

	case *types.RowExtend:
		var err error
		t.Labels.Range(func(label string, ts types.TypeList) bool {
			ts.Range(func(i int, t types.Type) bool {
				err = ctx.occursAdjustLevels(id, level, t)
				return err == nil
			})
			return err == nil
		})
		if err != nil {
			return err
		}
		return ctx.occursAdjustLevels(id, level, t.Row)

	default:
		return nil
	}
}

type UnifyTxn struct {
	Speculate           bool
	LinkStash           []StashedLink
	DeferredConstraints []DeferredConstraint
}

func (ctx *CommonContext) NewUnifyTxn() UnifyTxn {
	txn := UnifyTxn{ctx.Speculate, ctx.LinkStash, ctx.DeferredConstraints}
	ctx.Speculate = true
	return txn
}

func (ctx *CommonContext) Rollback(txn UnifyTxn) {
	ctx.UnstashLinks(len(ctx.LinkStash) - len(txn.LinkStash))
	ctx.Speculate, ctx.LinkStash, ctx.DeferredConstraints = txn.Speculate, txn.LinkStash, txn.DeferredConstraints
}

func (ctx *CommonContext) Commit(txn UnifyTxn) {
	ctx.Speculate, ctx.LinkStash = txn.Speculate, txn.LinkStash
}

func (ctx *CommonContext) CanUnify(a, b types.Type) bool {
	txn := ctx.NewUnifyTxn()
	err := ctx.Unify(a, b)
	ctx.Rollback(txn)
	return err == nil
}

func (ctx *CommonContext) TryUnify(a, b types.Type) error {
	txn := ctx.NewUnifyTxn()
	if err := ctx.Unify(a, b); err != nil {
		ctx.Rollback(txn)
		return err
	}
	ctx.Commit(txn)
	return nil
}

func (ctx *CommonContext) ApplyDeferredConstraints() (invalidExpr ast.Expr, err error) {
	ctx.CheckingDeferredConstraints = true
	for _, c := range ctx.DeferredConstraints {
		tv, link := c.Var, c.Var.Link()
		if len(tv.Constraints()) == 0 {
			continue
		}
		tv.UnsafeUnsetLink()
		if err = ctx.Unify(tv, ctx.Instantiate(tv.LevelNum(), link)); err != nil {
			invalidExpr = c.Expr
			break
		}
	}
	ctx.CheckingDeferredConstraints = false
	return
}

func (ctx *CommonContext) applyConstraints(a *types.Var, b types.Type) error {
	acs := a.Constraints()
	bv, bIsVar := b.(*types.Var)
	// Ensure size type-variables are only linked to size types (or other type-variables):
	if a.IsSizeVar() && !bIsVar {
		if _, ok := b.(types.Size); !ok {
			return errors.New("Failed to unify size type-variable with " + b.TypeName())
		}
	}
	// Ensure constructor/constant type-variables are only linked to type constants (or other type-variables):
	if a.IsConstVar() && !bIsVar {
		if _, ok := b.(*types.Const); !ok {
			return errors.New("Failed to unify constructor/constant type-variable with " + b.TypeName())
		}
	}
	if len(acs) == 0 {
		return nil
	}
	// If b is a type-variable, propagate instance constraints to b:
	if bIsVar {
		bcs := bv.Constraints()
		if ctx.Speculate {
			// don't modify the existing slice of constraints
			ctx.StashLink(bv)
			bcsTmp := make([]types.InstanceConstraint, len(bcs), len(bcs)+len(acs))
			copy(bcsTmp, bcs)
			bv.SetConstraints(bcsTmp)
		}
		for _, c := range acs {
			// Constraints which overlap or are subsumed will be eliminated:
			bv.AddConstraint(c)
		}
		a.SetConstraints(nil)
		return nil
	}
	// Eliminate instance constraints (find a matching instance for each type-class):
	for _, c := range acs {
		// Overlapping instances are detected when they are declared. Overlap is only allowed
		// between instances where one is a subclass of the other, and the search order ensures
		// sub-classes are visited first. If the linked type b unifies with multiple instances,
		// overlap will be re-checked after inference during deferred unification (when enabled).
		var firstMatch, lastMatch *types.Instance
		overlapping := false
		c.TypeClass.MatchInstance(b, func(inst *types.Instance) (done bool) {
			if ctx.CanUnify(b, ctx.Instantiate(a.LevelNum(), inst.Param)) {
				// Sub-classes are visited first:
				if lastMatch != nil && !lastMatch.TypeClass.HasSuperClass(inst.TypeClass) {
					overlapping = true
				}
				if firstMatch == nil {
					firstMatch = inst
				}
				lastMatch = inst
			}
			return overlapping
		})
		if firstMatch == nil {
			return errors.New("No matching instance found for type-class " + c.TypeClass.Name)
		}
		if overlapping {
			if ctx.CheckingDeferredConstraints || !ctx.DeferredConstraintsEnabled {
				return errors.New("Instance cannot be determined from the context for type-class " + c.TypeClass.Name)
			}
			// Deferred constraints are applied after inference. Type-variables within the linked type b
			// should not be generalized, to ensure constraints propagate during the deferred unification:
			const forceGeneralize, weak = false, true
			GeneralizeOpts(a.Level(), b, forceGeneralize, weak)
			ctx.DeferredConstraints = append(ctx.DeferredConstraints, DeferredConstraint{a, ctx.CurrentExpr})
			continue
		}
		// If only one matching instance is found, it can be safely unified with the candidate type (err should always be nil):
		if err := ctx.Unify(b, ctx.Instantiate(a.LevelNum(), firstMatch.Param)); err != nil {
			return err
		}
	}
	return nil
}

func (ctx *CommonContext) Unify(a, b types.Type) error {
	// Path compression:
	a, b = types.RealType(a), types.RealType(b)

	if a == b {
		return nil
	}

	// unify with recursive types:

	if a, ok := a.(*types.RecursiveLink); ok {
		if b, ok := b.(*types.RecursiveLink); ok {
			if !a.Recursive.Matches(b.Recursive) || a.Index != b.Index {
				return errors.New("Failed to unify recursive type links")
			}
			// All unifiable type-variables should occur within the recursive group's type-parameters.
			for i, tv := range a.Recursive.Params {
				if err := ctx.Unify(tv, b.Recursive.Params[i]); err != nil {
					return err
				}
			}
			return nil
		}
		// unroll a
		return ctx.Unify(a.Link(), b)
	}
	if b, ok := b.(*types.RecursiveLink); ok {
		// unroll b
		return ctx.Unify(a, b.Link())
	}

	// unify type variables:

	avar, _ := a.(*types.Var)
	bvar, _ := b.(*types.Var)
	switch {
	case avar == nil && bvar != nil:
		return ctx.Unify(b, a)

	case avar != nil:
		if avar.IsGenericVar() {
			return errors.New("Generic type-variable was not instantiated before unification")
		}
		// weak or unbound
		if ctx.Speculate {
			ctx.StashLink(avar)
		}
		if bvar != nil {
			if bvar.IsUnboundVar() && avar.Id() == bvar.Id() {
				return errors.New("Implicitly recursive types are not supported")
			}
			if ctx.Speculate {
				ctx.StashLink(bvar)
			}
			// propagate the weak flag bi-directionally:
			if avar.IsWeakVar() && !bvar.IsWeakVar() {
				bvar.SetWeak()
			} else if !avar.IsWeakVar() && bvar.IsWeakVar() {
				avar.SetWeak()
			}
			// propagate the size flag bi-directionally:
			switch {
			case avar.IsRestrictedVar() && bvar.IsRestrictedVar():
				if avar.RestrictedLevel() != bvar.RestrictedLevel() {
					return errors.New("Failed to unify type-variables with different restrictions")
				}
			case avar.IsRestrictedVar():
				bvar.Restrict(avar.Level())
			case bvar.IsRestrictedVar():
				avar.Restrict(bvar.Level())
			}
		}
		// prevent cyclical types:
		if err := ctx.occursAdjustLevels(avar.Id(), avar.LevelNum(), b); err != nil {
			return err
		}
		// propagate or eliminate type-class constraints:
		if err := ctx.applyConstraints(avar, b); err != nil {
			return err
		}

		avar.SetLink(b)
		return nil
	}

	// unify aliased types:

	aliasA, _ := a.(*types.App)
	aliasB, _ := b.(*types.App)
	var underA, underB types.Type
	if aliasA != nil && aliasA.Underlying != nil {
		underA = aliasA.Underlying
	}
	if aliasB != nil && aliasB.Underlying != nil {
		underB = aliasB.Underlying
	}
	switch {
	case underA != nil && underB == nil: // only a is an alias
		// If b is a type application, Const and Args are unified in the main switch below
		if aliasB == nil {
			// unify b with a's underlying type
			return ctx.Unify(underA, b)
		}
	case underA == nil && underB != nil: // only b is an alias
		// If b is a type application, Const and Args are unified in the main switch below
		if aliasA == nil {
			// unify b with a's underlying type
			return ctx.Unify(underB, a)
		}
		// a is a type application; swapping a/b will fallthrough to the case above
		return ctx.Unify(b, a)
	case underA != nil: // both are aliases
		// Const and Args are unified in the main switch below
		if err := ctx.Unify(underA, underB); err != nil {
			return err
		}
	}

	// unify types:

	switch a := a.(type) {
	// Var and RecursiveLink are handled above

	case *types.Unit:
		if _, ok := b.(*types.Unit); ok {
			return nil
		}
		return errors.New("Failed to unify unit with " + types.TypeName(b))

	case *types.Const:
		if b, ok := b.(*types.Const); ok {
			if a.Name == b.Name {
				return nil
			}
			return errors.New("Failed to unify " + a.Name + " with " + b.Name)
		}

	case types.Size:
		if b, ok := b.(types.Size); ok {
			if a != b {
				return errors.New("Failed to unify sized types with different sizes")
			}
			return nil
		}

	case *types.App:
		bapp, ok := b.(*types.App)
		if !ok {
			return errors.New("Failed to unify type-application with " + types.TypeName(b))
		}
		if err := ctx.Unify(a.Const, bapp.Const); err != nil {
			return err
		}
		if len(a.Params) != len(bapp.Params) {
			return errors.New("Cannot unify type-applications with differing arity")
		}
		for i := range a.Params {
			if err := ctx.Unify(a.Params[i], bapp.Params[i]); err != nil {
				return err
			}
		}
		if underA != nil && aliasB != nil {
			aliasB.Underlying = underA
		}
		return nil

	case *types.Arrow:
		b, ok := b.(*types.Arrow)
		if !ok {
			return errors.New("Failed to unify arrow with type " + types.TypeName(b))
		}
		if len(a.Args) != len(b.Args) {
			return errors.New("Cannot unify arrows with differing arity")
		}
		for i := range a.Args {
			if err := ctx.Unify(a.Args[i], b.Args[i]); err != nil {
				return err
			}
		}
		if err := ctx.Unify(a.Return, b.Return); err != nil {
			return err
		}
		return nil

	case *types.Record:
		if b, ok := b.(*types.Record); ok {
			return ctx.Unify(a.Row, b.Row)
		}

	case *types.Variant:
		if b, ok := b.(*types.Variant); ok {
			return ctx.Unify(a.Row, b.Row)
		}

	case *types.RowExtend:
		if b, ok := b.(*types.RowExtend); ok {
			return ctx.unifyRows(a, b)
		}

	case *types.RowEmpty:
		if _, ok := b.(*types.RowEmpty); ok {
			return nil
		}

	}

	return errors.New("Failed to unify " + types.TypeName(a) + " with " + types.TypeName(b))
}

// Returns a unification error or extra types in a and b, respectively, if the lengths do not match.
// Lists are unified from higher indexes to lower indexes, starting at the highest index of each list:
// [0: a, 1: b, 2: c, 3: d] ==> extra types in a: [0: a]
//       [0: b, 1: c, 2: d] ==> extra types in b: []
func (ctx *CommonContext) unifyLists(a, b types.TypeList) (extraA, extraB types.TypeList, err error) {
	la, lb := a.Len(), b.Len()
	// common case for unscoped labels:
	if la == 1 && lb == 1 {
		return types.EmptyTypeList, types.EmptyTypeList, ctx.Unify(a.Get(0), b.Get(0))
	}
	n := la
	if lb > la {
		extraA, extraB = types.EmptyTypeList, b.Slice(0, lb-la)
		// rearrange so the longer list is on the left-hand side:
		n, a, b, la, lb = lb, b, a, lb, la
	} else if la > lb {
		extraA, extraB = a.Slice(0, la-lb), types.EmptyTypeList
	} else {
		extraA, extraB = types.EmptyTypeList, types.EmptyTypeList
	}
	ai, bi := la-lb, 0
	for ai < n {
		va, vb := a.Get(ai), b.Get(bi)
		if err := ctx.Unify(va, vb); err != nil {
			return types.EmptyTypeList, types.EmptyTypeList, err
		}
		ai++
		bi++
	}
	return extraA, extraB, nil
}

func (ctx *CommonContext) unifyRows(a, b types.Type) error {
	labelsA, restA, err := types.FlattenRowType(a)
	if err != nil {
		return err
	}
	labelsB, restB, err := types.FlattenRowType(b)
	if err != nil {
		return err
	}

	// labels missing from labelsA/labelsA:
	var missingA, missingB types.TypeMapBuilder
	iterA, iterB := labelsA.Iterator(), labelsB.Iterator()
	for !iterA.Done() {
		label, va := iterA.Next()
		if _, ok := labelsA.Get(label); !ok {
			missingB.EnsureInitialized()
			missingB.Set(label, va)
		}
	}
	for !iterB.Done() {
		label, vb := iterB.Next()
		va, ok := labelsA.Get(label)
		if !ok {
			missingA.EnsureInitialized()
			missingA.Set(label, vb)
			continue
		}
		// extra types in a and b, respectively, if the lengths do not match:
		extraA, extraB, err := ctx.unifyLists(va, vb)
		if err != nil {
			return err
		}
		if extraA.Len() > 0 {
			missingB.EnsureInitialized()
			missingB.Set(label, extraA)
		}
		if extraB.Len() > 0 {
			missingA.EnsureInitialized()
			missingA.Set(label, extraB)
		}
	}

	za, zb := missingA.Len() == 0, missingB.Len() == 0
	switch {
	case za && zb: // all labels match
		return ctx.Unify(restA, restB)
	case za && !zb: // labels missing in labelsB
		return ctx.Unify(restB, &types.RowExtend{Row: restA, Labels: missingB.Build()})
	case !za && zb: // labels missing in labelsA
		return ctx.Unify(restA, &types.RowExtend{Row: restB, Labels: missingA.Build()})
	default: // labels missing in both labelsA/labelsB
		switch restA := restA.(type) {
		case *types.RowEmpty:
			// will result in an error:
			return ctx.Unify(restA, &types.RowExtend{Row: ctx.VarTracker.New(0), Labels: missingA.Build()})
		case *types.Var:
			if !restA.IsUnboundVar() {
				return errors.New("Invalid state while unifying type-variables for rows")
			}
			tv := ctx.VarTracker.New(restA.LevelNum())
			ext := types.RowExtend{Row: tv, Labels: missingB.Build()}
			if err := ctx.Unify(restB, &ext); err != nil {
				return err
			}
			if restA.IsLinkVar() {
				return errors.New("Invalid recursive row-types")
			}
			ext = types.RowExtend{Row: tv, Labels: missingA.Build()}
			return ctx.Unify(restA, &ext)
		}
	}

	return errors.New("Invalid state while unifying rows")
}

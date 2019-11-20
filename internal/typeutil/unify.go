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

	"github.com/wdamron/poly/types"
)

// See "Efficient Generalization with Levels" (Oleg Kiselyov) -- http://okmij.org/ftp/ML/generalization.html#levels
func (ctx *CommonContext) occursAdjustLevels(id, level int, t types.Type) error {
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
			if t.Level() > level {
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
		for _, arg := range t.Args {
			if err := ctx.occursAdjustLevels(id, level, arg); err != nil {
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

func (ctx *CommonContext) CanUnify(a, b types.Type) bool {
	speculating := ctx.Speculate
	ctx.Speculate = true
	stashedLinks := ctx.LinkStash
	err := ctx.Unify(a, b)
	ctx.UnstashLinks(len(ctx.LinkStash) - len(stashedLinks))
	ctx.Speculate, ctx.LinkStash = speculating, stashedLinks
	return err == nil
}

func (ctx *CommonContext) TryUnify(a, b types.Type) error {
	speculating := ctx.Speculate
	ctx.Speculate = true
	stashedLinks := ctx.LinkStash
	err := ctx.Unify(a, b)
	if err != nil {
		ctx.UnstashLinks(len(ctx.LinkStash) - len(stashedLinks))
	}
	ctx.Speculate, ctx.LinkStash = speculating, stashedLinks
	return err
}

func (ctx *CommonContext) applyConstraints(a *types.Var, b types.Type) error {
	aConstraints := a.Constraints()
	bv, bIsVar := b.(*types.Var)
	// Ensure size type-variables are only linked to size types (or other type-variables):
	if a.IsSizeVar() && !bIsVar {
		if _, ok := b.(types.Size); !ok {
			return errors.New("Failed to unify size type-variable with " + b.TypeName())
		}
	}
	if len(aConstraints) == 0 {
		return nil
	}
	// If b is a type-variable, propagate instance constraints to b:
	if bIsVar {
		bConstraints := bv.Constraints()
		if ctx.Speculate {
			// don't modify the existing slice of constraints
			ctx.StashLink(bv)
			bConstraintsTmp := make([]types.InstanceConstraint, len(bConstraints), len(bConstraints)+len(aConstraints))
			copy(bConstraintsTmp, bConstraints)
			bv.SetConstraints(bConstraintsTmp)
		}
		for _, c := range aConstraints {
			bv.AddConstraint(c)
		}
		a.SetConstraints(nil)
		return nil
	}

	// Eliminate instance constraints (find a matching instance for each type-class)
	for _, c := range aConstraints {
		var matched *types.Instance
		if ctx.LastInstanceMatch != nil {
			if inst, ok := ctx.LastInstanceMatch[c.TypeClass.Id]; ok && ctx.CanUnify(b, ctx.Instantiate(a.Level(), inst.Param)) {
				matched = inst
			}
		}
		if matched == nil {
			c.TypeClass.MatchInstance(b, func(inst *types.Instance) (found bool) {
				if err := ctx.TryUnify(b, ctx.Instantiate(a.Level(), inst.Param)); err == nil {
					matched = inst
					return true
				}
				return false
			})
		}
		if matched == nil {
			return errors.New("No matching instance found for type-class " + c.TypeClass.Name)
		}
		if ctx.LastInstanceMatch == nil {
			ctx.LastInstanceMatch = make(map[int]*types.Instance)
		}
		ctx.LastInstanceMatch[c.TypeClass.Id] = matched
	}
	return nil
}

func (ctx *CommonContext) Unify(a, b types.Type) error {
	a, b = types.RealType(a), types.RealType(b)
	if a == b {
		return nil
	}

	// unify with recursive types:

	if a, ok := a.(*types.RecursiveLink); ok {
		if b, ok := b.(*types.RecursiveLink); ok {
			if a.Recursive.Id != b.Recursive.Id || a.Index != b.Index {
				return errors.New("Failed to unify recursive type links")
			}
			as, bs := a.Recursive.Types, b.Recursive.Types
			// All unifiable type-variables should occur within the constructor and type-parameters.
			// The underlying type should be ignored (to prevent possible reassignment of underlying types).
			var appA, appB types.App
			for i, t := range as {
				appA.Const, appA.Args = t.Const, t.Args
				appB.Const, appB.Args = bs[i].Const, bs[i].Args
				if err := ctx.Unify(&appA, &appB); err != nil {
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
			if avar.IsSizeVar() && !bvar.IsSizeVar() {
				bvar.RestrictSizeVar()
			} else if !avar.IsSizeVar() && bvar.IsSizeVar() {
				avar.RestrictSizeVar()
			}
		} else if avar.IsWeakVar() {
			// make b weak
			types.MarkWeak(b)
		}
		// prevent cyclical types:
		if err := ctx.occursAdjustLevels(avar.Id(), avar.Level(), b); err != nil {
			return err
		}
		// propagate or eliminate type-class constraints:
		if err := ctx.applyConstraints(avar, b); err != nil {
			return err
		}

		if b, ok := b.(*types.App); ok && b.IsWeak() {
			avar.SetWeak()
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
		b, ok := b.(*types.App)
		if !ok {
			return errors.New("Failed to unify type-application with " + b.TypeName())
		}
		if err := ctx.Unify(a.Const, b.Const); err != nil {
			return err
		}
		if len(a.Args) != len(b.Args) {
			return errors.New("Cannot unify type-applications with differing arity")
		}
		for i := range a.Args {
			if err := ctx.Unify(a.Args[i], b.Args[i]); err != nil {
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
			return errors.New("Failed to unify arrow with type " + b.TypeName())
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

// returns a unification error or extra types in a and b, respectively, if the lengths do not match
func (ctx *CommonContext) unifyLists(a, b types.TypeList) (extraA, extraB types.TypeList, err error) {
	la, lb := a.Len(), b.Len()
	swapped := false
	if lb > la {
		a, b, la, lb = b, a, lb, la
		swapped = true
	}
	extraA, extraB = a.Slice(0, la-lb), types.EmptyTypeList
	ai, bi := la-lb, 0
	n := a.Len()
	for ai < n {
		va, vb := a.Get(ai), b.Get(bi)
		if err := ctx.Unify(va, vb); err != nil {
			return types.EmptyTypeList, types.EmptyTypeList, err
		}
		ai++
		bi++
	}
	if swapped {
		extraA, extraB = extraB, extraA
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
			tv := ctx.VarTracker.New(restA.Level())
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

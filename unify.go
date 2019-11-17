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

	"github.com/wdamron/poly/internal/util"
	"github.com/wdamron/poly/types"
)

func (ctx *commonContext) occursAdjustLevels(id, level int, t types.Type) error {
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
				if ctx.speculate {
					ctx.stashLink(t)
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

func (ctx *commonContext) canUnify(a, b types.Type) bool {
	speculating := ctx.speculate
	ctx.speculate = true
	stashedLinks := ctx.linkStash
	err := ctx.unify(a, b)
	ctx.unstashLinks(len(ctx.linkStash) - len(stashedLinks))
	ctx.speculate, ctx.linkStash = speculating, stashedLinks
	return err == nil
}

func (ctx *commonContext) applyConstraints(a *types.Var, b types.Type) error {
	aConstraints := a.Constraints()
	if len(aConstraints) == 0 {
		return nil
	}
	bv, bIsVar := b.(*types.Var)
	if bIsVar {
		bConstraints := bv.Constraints()
		// merge instance constraints into the link target
		if ctx.speculate {
			// don't modify the existing slice of constraints
			ctx.stashLink(bv)
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

	// evaluate instance constraints (find a matching instance for each type-class)
	seen := util.NewIntDedupeMap()
	for _, c := range aConstraints {
		if seen[c.TypeClass.Id] {
			continue
		}
		seen[c.TypeClass.Id] = true
		var matched *types.Instance
		if ctx.lastInstanceMatch != nil {
			if inst, ok := ctx.lastInstanceMatch[c.TypeClass.Id]; ok && ctx.canUnify(b, ctx.instantiate(a.Level(), inst.Param)) {
				matched = inst
			}
		}
		if matched == nil {
			c.TypeClass.MatchInstance(b, func(inst *types.Instance) (found bool) {
				if ctx.canUnify(b, ctx.instantiate(a.Level(), inst.Param)) {
					matched = inst
					return true
				}
				return false
			})
		}
		if matched == nil {
			seen.Release()
			return errors.New("No matching instance found for type-class " + c.TypeClass.Name)
		}
		if ctx.lastInstanceMatch == nil {
			ctx.lastInstanceMatch = make(map[int]*types.Instance)
		}
		ctx.lastInstanceMatch[c.TypeClass.Id] = matched
	}
	seen.Release()
	return nil
}

func (ctx *commonContext) unify(a, b types.Type) error {
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
			return nil
		}
		// unroll a
		return ctx.unify(a.Link(), b)
	}
	if b, ok := b.(*types.RecursiveLink); ok {
		// unroll b
		return ctx.unify(a, b.Link())
	}

	// unify type variables:

	avar, _ := a.(*types.Var)
	bvar, _ := b.(*types.Var)
	switch {
	case avar == nil && bvar != nil:
		return ctx.unify(b, a)

	case avar != nil:
		if avar.IsGenericVar() {
			return errors.New("Generic type-variable was not instantiated before unification")
		}
		// weak or unbound
		if ctx.speculate {
			ctx.stashLink(avar)
		}
		if bvar != nil {
			if bvar.IsUnboundVar() && avar.Id() == bvar.Id() {
				return errors.New("Implicitly recursive types are not supported")
			}
			if avar.IsWeakVar() || bvar.IsWeakVar() {
				avar.SetWeak()
				bvar.SetWeak()
			}
		} else if avar.IsWeakVar() {
			// make b weak
			types.MarkWeak(b)
		}
		if err := ctx.occursAdjustLevels(avar.Id(), avar.Level(), b); err != nil {
			return err
		}
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
	case underA == nil && underB != nil:
		return ctx.unify(b, a)
	case underB != nil: // both are aliases
		// Const and Args are unified in the main switch below
		if err := ctx.unify(underA, underB); err != nil {
			return err
		}
	case underA != nil: // only a is an alias
		// If b is a type application, Const and Args are unified in the main switch below
		if aliasB == nil {
			// unify b with a's underlying type
			return ctx.unify(underA, b)
		}
	}

	// unify types:

	switch a := a.(type) {
	case *types.Const:
		if b, ok := b.(*types.Const); ok {
			if a.Name == b.Name {
				return nil
			}
			return errors.New("Failed to unify " + a.Name + " with " + b.Name)
		}

	case *types.App:
		b, ok := b.(*types.App)
		if !ok {
			return errors.New("Failed to unify type-application with " + b.TypeName())
		}
		if err := ctx.unify(a.Const, b.Const); err != nil {
			return err
		}
		if len(a.Args) != len(b.Args) {
			return errors.New("Cannot unify type-applications with differing arity")
		}
		for i := range a.Args {
			if err := ctx.unify(a.Args[i], b.Args[i]); err != nil {
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
			if err := ctx.unify(a.Args[i], b.Args[i]); err != nil {
				return err
			}
		}
		if err := ctx.unify(a.Return, b.Return); err != nil {
			return err
		}
		return nil

	case *types.Record:
		if b, ok := b.(*types.Record); ok {
			return ctx.unify(a.Row, b.Row)
		}

	case *types.Variant:
		if b, ok := b.(*types.Variant); ok {
			return ctx.unify(a.Row, b.Row)
		}

	case *types.RowExtend:
		if b, ok := b.(*types.RowExtend); ok {
			return ctx.unifyRows(a, b)
		}

	case *types.RowEmpty:
		if _, ok := b.(*types.RowEmpty); ok {
			return nil
		}

	case *types.RecursiveLink:
		panic("unreachable")
	}

	return errors.New("Failed to unify " + a.TypeName() + " with " + b.TypeName())
}

func (ctx *commonContext) unifyLists(a, b types.TypeList) (xa, xb types.TypeList, err error) {
	i := 0
	n := a.Len()
	if b.Len() < n {
		n = b.Len()
	}
	for i < n {
		va, vb := a.Get(i), b.Get(i)
		if err := ctx.unify(va, vb); err != nil {
			return types.EmptyTypeList, types.EmptyTypeList, err
		}
		i++
	}
	return a.Slice(i, a.Len()), b.Slice(i, b.Len()), nil
}

func (ctx *commonContext) unifyRows(a, b types.Type) error {
	ma, ra, err := types.FlattenRowType(a)
	if err != nil {
		return err
	}
	mb, rb, err := types.FlattenRowType(b)
	if err != nil {
		return err
	}

	// labels missing from ma/mb
	xa, xb := types.NewTypeMapBuilder(), types.NewTypeMapBuilder()
	ia, ib := ma.Iterator(), mb.Iterator()
	for !ia.Done() {
		la, va := ia.Next()
		if _, ok := mb.Get(la); !ok {
			xb.Set(la, va)
		}
	}
	for !ib.Done() {
		lb, vb := ib.Next()
		va, ok := ma.Get(lb)
		if !ok {
			xa.Set(lb, vb)
			continue
		}
		ua, ub, err := ctx.unifyLists(va, vb)
		if err != nil {
			return err
		}
		if ua.Len() > 0 {
			xb.Set(lb, ua)
		}
		if ub.Len() > 0 {
			xa.Set(lb, ub)
		}
	}

	za, zb := xa.Len() == 0, xb.Len() == 0
	switch {
	case za && zb: // all labels match
		return ctx.unify(ra, rb)
	case za && !zb: // labels missing in mb
		return ctx.unify(rb, &types.RowExtend{Row: ra, Labels: xb.Build()})
	case !za && zb: // labels missing in ma
		return ctx.unify(ra, &types.RowExtend{Row: rb, Labels: xa.Build()})
	default: // labels missing in both ma/mb
		switch ra := ra.(type) {
		case *types.RowEmpty:
			// will result in an error:
			return ctx.unify(ra, &types.RowExtend{Row: ctx.varTracker.New(0), Labels: xa.Build()})
		case *types.Var:
			if !ra.IsUnboundVar() {
				return errors.New("Invalid state while unifying type-variables for rows")
			}
			tv := ctx.varTracker.New(ra.Level())
			if err := ctx.unify(rb, &types.RowExtend{Row: tv, Labels: xb.Build()}); err != nil {
				return err
			}
			if ra.IsLinkVar() {
				return errors.New("Invalid recursive row-types")
			}
			return ctx.unify(ra, &types.RowExtend{Row: tv, Labels: xa.Build()})
		}
	}

	return errors.New("Invalid state while unifying rows")
}

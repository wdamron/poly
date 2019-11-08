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
		case t.IsUnboundVar():
			if t.Id() == id {
				return errors.New("Recursive types are not supported")
			}
			if t.Level() > level {
				if ctx.speculate {
					ctx.stashLink(t)
				}
				t.SetLevel(level)
			}
		}
		return nil

	case *types.App:
		if err := ctx.occursAdjustLevels(id, level, t.Const); err != nil {
			return err
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

func (ctx *commonContext) tryUnify(a, b types.Type) error {
	speculating := ctx.speculate
	ctx.speculate = true
	stashedLinks := ctx.linkStash
	err := ctx.unify(a, b)
	if err != nil {
		ctx.unstashLinks(len(ctx.linkStash) - len(stashedLinks))
	}
	ctx.speculate, ctx.linkStash = speculating, stashedLinks
	return err
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

func (ctx *commonContext) unify(a, b types.Type) error {
	if a == b {
		return nil
	}

	if a, ok := a.(*types.Var); ok {
		switch {
		case a.IsLinkVar():
			return ctx.unify(a.Link(), b)
		case a.IsUnboundVar():
			b = types.RealType(b)
			bv, bIsVar := b.(*types.Var)
			if bIsVar {
				if bv.IsUnboundVar() && a.Id() == bv.Id() {
					return errors.New("Recursive types are not supported")
				}
			}
			if ctx.speculate {
				ctx.stashLink(a)
			}
			if err := ctx.occursAdjustLevels(a.Id(), a.Level(), b); err != nil {
				return err
			}
			aConstraints := a.Constraints()
			if len(aConstraints) != 0 {
				if bIsVar {
					bConstraints := bv.Constraints()
					// propagate instance constraints to the link target
					if ctx.speculate {
						ctx.stashLink(bv)
						bConstraints2 := make([]types.InstanceConstraint, len(aConstraints)+len(bConstraints))
						copy(bConstraints2, aConstraints)
						copy(bConstraints2[len(aConstraints):], bConstraints)
						bv.SetConstraints(bConstraints2)
					} else {
						bv.SetConstraints(append(bConstraints, aConstraints...))
					}
					a.SetConstraints(nil)
				} else {
					// evaluate instance constraints (find a matching instance for each type-class)
					seen := util.NewDedupeMap()
					for _, c := range aConstraints {
						if seen[c.TypeClass.Name] {
							continue
						}
						seen[c.TypeClass.Name] = true
						found := c.TypeClass.FindInstance(func(inst *types.Instance) (found bool) {
							if err := ctx.tryUnify(b, ctx.instantiate(a.Level(), inst.Param)); err != nil {
								return false
							}
							return true
						})
						if !found {
							seen.Release()
							return errors.New("No matching instance found for type-class " + c.TypeClass.Name)
						}
					}
					seen.Release()
				}

			}

			a.SetLink(b)
			return nil
		default:
			return errors.New("Generic type-variable was not instantiated before unification")
		}
	}

	if b, ok := b.(*types.Var); ok {
		return ctx.unify(b, a)
	}

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
			return errors.New("Cannot unify function calls with differing arity")
		}
		for i := range a.Args {
			if err := ctx.unify(a.Args[i], b.Args[i]); err != nil {
				return err
			}
		}
		return nil

	case *types.Arrow:
		b, ok := b.(*types.Arrow)
		if !ok {
			return errors.New("Failed to unify arrow with type")
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

	xa, xb := types.NewTypeMapBuilder(), types.NewTypeMapBuilder()
	ia, ib := ma.Iterator(), mb.Iterator()

UnifyLabels:
	for {
		switch {
		case ia.Done() && ib.Done():
			break UnifyLabels
		case ia.Done():
			for !ib.Done() {
				xa.Set(ib.Next())
			}
			break UnifyLabels
		case ib.Done():
			for !ia.Done() {
				xb.Set(ia.Next())
			}
			break UnifyLabels
		}

		la, va := ia.Peek()
		lb, vb := ib.Peek()
		switch {
		case la > lb:
			xa.Set(lb, vb)
			ib.Next()
		case la < lb:
			xb.Set(la, va)
			ia.Next()
		default:
			ua, ub, err := ctx.unifyLists(va, vb)
			if err != nil {
				return err
			}
			if ua.Len() > 0 {
				xb.Set(la, ua)
			}
			if ub.Len() > 0 {
				xa.Set(lb, ub)
			}
			ia.Next()
			ib.Next()
		}
	}

	za, zb := xa.Len() == 0, xb.Len() == 0
	switch {
	case za && zb:
		return ctx.unify(ra, rb)
	case za && !zb:
		return ctx.unify(rb, &types.RowExtend{Row: ra, Labels: xb.Build()})
	case !za && zb:
		return ctx.unify(ra, &types.RowExtend{Row: rb, Labels: xa.Build()})
	default:
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

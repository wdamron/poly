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

	"github.com/wdamron/poly/types"
)

func (ti *Inference) occursAdjustLevels(id, level int, t types.Type) error {
	switch t := t.(type) {
	case *types.Var:
		switch {
		case t.IsLinkVar():
			return ti.occursAdjustLevels(id, level, t.Link())
		case t.IsGenericVar():
			return errors.New("Invalid occurrance check within generic type-variable")
		case t.IsUnboundVar():
			if t.Id() == id {
				return errors.New("Invalid recursive types")
			}
			if t.Level() > level {
				t.AdjustLevel(level)
			}
		}
		return nil

	case *types.App:
		if err := ti.occursAdjustLevels(id, level, t.Func); err != nil {
			return err
		}
		for _, arg := range t.Args {
			if err := ti.occursAdjustLevels(id, level, arg); err != nil {
				return err
			}
		}
		return nil

	case *types.Arrow:
		for _, arg := range t.Args {
			if err := ti.occursAdjustLevels(id, level, arg); err != nil {
				return err
			}
		}
		return ti.occursAdjustLevels(id, level, t.Return)

	case *types.Record:
		return ti.occursAdjustLevels(id, level, t.Row)

	case *types.Variant:
		return ti.occursAdjustLevels(id, level, t.Row)

	case *types.RowExtend:
		var err error
		t.Labels.Range(func(label string, ts types.TypeList) bool {
			ts.Range(func(i int, t types.Type) bool {
				err = ti.occursAdjustLevels(id, level, t)
				return err == nil
			})
			return err == nil
		})
		if err != nil {
			return err
		}
		return ti.occursAdjustLevels(id, level, t.Row)

	default:
		return nil
	}
}

func (ti *Inference) unify(a, b types.Type) error {
	if a == b {
		return nil
	}

	if a, ok := a.(*types.Var); ok {
		switch {
		case a.IsLinkVar():
			return ti.unify(a.Link(), b)
		case a.IsUnboundVar():
			if b, ok := b.(*types.Var); ok {
				if b.IsUnboundVar() && a.Id() == b.Id() {
					return errors.New("Cannot unify pair of unbound type-variables")
				}
			}
			if err := ti.occursAdjustLevels(a.Id(), a.Level(), b); err != nil {
				return err
			}
			a.SetLink(b)
			return nil
		default:
			return errors.New("Generic type-variable was not generalized or instantiated before unification")
		}
	}

	if b, ok := b.(*types.Var); ok {
		return ti.unify(b, a)
	}

	switch a := a.(type) {
	case *types.Const:
		if b, ok := b.(*types.Const); ok {
			if a.Name == b.Name {
				return nil
			}
			return errors.New("Failed to unify Const " + a.Name + " with Const " + b.Name)
		}

	case *types.App:
		b, ok := b.(*types.App)
		if !ok {
			return errors.New("Failed to unify function call with type")
		}
		if err := ti.unify(a.Func, b.Func); err != nil {
			return err
		}
		if len(a.Args) != len(b.Args) {
			return errors.New("Cannot unify function calls with differing arity")
		}
		for i := range a.Args {
			if err := ti.unify(a.Args[i], b.Args[i]); err != nil {
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
			if err := ti.unify(a.Args[i], b.Args[i]); err != nil {
				return err
			}
		}
		if err := ti.unify(a.Return, b.Return); err != nil {
			return err
		}
		return nil

	case *types.Record:
		if b, ok := b.(*types.Record); ok {
			return ti.unify(a.Row, b.Row)
		}

	case *types.Variant:
		if b, ok := b.(*types.Variant); ok {
			return ti.unify(a.Row, b.Row)
		}

	case *types.RowExtend:
		if b, ok := b.(*types.RowExtend); ok {
			return ti.unifyRows(a, b)
		}

	case types.RowEmpty:
		if _, ok := b.(types.RowEmpty); ok {
			return nil
		}
	}

	return errors.New("Failed to unify " + a.TypeName() + " with " + b.TypeName())
}

func (ti *Inference) unifyLists(a, b types.TypeList) (xa, xb types.TypeList, err error) {
	i := 0
	n := a.Len()
	if b.Len() < n {
		n = b.Len()
	}
	for i < n {
		va, vb := a.Get(i), b.Get(i)
		if err := ti.unify(va, vb); err != nil {
			return types.EmptyTypeList, types.EmptyTypeList, err
		}
		i++
	}
	return a.Slice(i, a.Len()), b.Slice(i, b.Len()), nil
}

func (ti *Inference) unifyRows(a, b types.Type) error {
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
			ua, ub, err := ti.unifyLists(va, vb)
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
		return ti.unify(ra, rb)
	case za && !zb:
		return ti.unify(rb, &types.RowExtend{Row: ra, Labels: xb.Build()})
	case !za && zb:
		return ti.unify(ra, &types.RowExtend{Row: rb, Labels: xa.Build()})
	default:
		switch ra := ra.(type) {
		case types.RowEmpty:
			// will result in an error:
			return ti.unify(ra, &types.RowExtend{Row: ti.newVar(0), Labels: xa.Build()})
		case *types.Var:
			if !ra.IsUnboundVar() {
				return errors.New("Invalid state while unifying type variables for rows")
			}
			tv := ti.newVar(ra.Level())
			if err := ti.unify(rb, &types.RowExtend{Row: tv, Labels: xb.Build()}); err != nil {
				return err
			}
			if ra.IsLinkVar() {
				return errors.New("Invalid recursive row-types")
			}
			return ti.unify(ra, &types.RowExtend{Row: tv, Labels: xa.Build()})
		}
	}

	return errors.New("Invalid state while unifying rows")
}

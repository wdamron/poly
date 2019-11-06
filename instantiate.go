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
	"github.com/wdamron/poly/types"
)

func (ctx *commonContext) instantiate(level int, t types.Type) types.Type {
	if !t.IsGeneric() {
		return t
	}

	switch t := t.(type) {
	case *types.Var:
		id := t.Id()
		if tv, ok := ctx.instLookup[id]; ok {
			return tv
		}
		tv := ctx.varTracker.New(level)
		constraints := t.Constraints()
		constraints2 := make([]types.InstanceConstraint, len(constraints))
		copy(constraints2, constraints)
		tv.SetConstraints(constraints2)
		ctx.instLookup[id] = tv
		return tv

	case *types.App:
		args := make([]types.Type, len(t.Args))
		for i, arg := range t.Args {
			args[i] = ctx.instantiate(level, arg)
		}
		return &types.App{Const: ctx.instantiate(level, t.Const), Args: args}

	case *types.Arrow:
		args := make([]types.Type, len(t.Args))
		for i, arg := range t.Args {
			args[i] = ctx.instantiate(level, arg)
		}
		return &types.Arrow{Args: args, Return: ctx.instantiate(level, t.Return), Method: t.Method}

	case *types.Method:
		arrow := ctx.instantiate(level, t.TypeClass.Methods[t.Name]).(*types.Arrow)
		arrow.Method = t
		return arrow

	case *types.Record:
		return &types.Record{Row: ctx.instantiate(level, t.Row)}

	case *types.Variant:
		return &types.Variant{Row: ctx.instantiate(level, t.Row)}

	case *types.RowExtend:
		m := t.Labels
		mb := m.Builder()
		m.Range(func(label string, ts types.TypeList) bool {
			lb := ts.Builder()
			ts.Range(func(i int, t types.Type) bool {
				lb.Set(i, ctx.instantiate(level, t))
				return true
			})
			mb.Set(label, lb.Build())
			return true
		})
		return &types.RowExtend{Row: ctx.instantiate(level, t.Row), Labels: mb.Build()}
	}
	panic("unexpected generic type " + t.TypeName())
}

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

func generalize(level int, t types.Type) types.Type {
	counter := 0
	t = visitType(0, t, types.ContainsRefs, &counter)
	if counter > 0 {
		// types containing mutable references must not be generalized
		return t
	}
	counter = 0
	return visitType(level, t, types.ContainsGenericVars, &counter)
}

func visitType(level int, t types.Type, flag types.TypeFlags, counter *int) types.Type {
	switch t := t.(type) {
	case *types.Var:
		switch {
		case t.IsUnboundVar():
			if flag == types.ContainsGenericVars {
				if t.Level() > level {
					*counter++
					t.SetGeneric()
				}
			}
			return t
		case t.IsLinkVar():
			return visitType(level, t.Link(), flag, counter)
		default:
			if flag == types.ContainsGenericVars {
				*counter++
			}
			return t
		}

	case *types.Ref:
		t.Deref = visitType(level, t.Deref, flag, counter)
		if flag == types.ContainsRefs {
			*counter++
		}
		return t

	case *types.App:
		count := *counter
		for i, arg := range t.Args {
			t.Args[i] = visitType(level, arg, flag, counter)
		}
		t.Const = visitType(level, t.Const, flag, counter)
		if *counter > count {
			t.Flags |= flag
		}
		return t

	case *types.Arrow:
		count := *counter
		for i, arg := range t.Args {
			t.Args[i] = visitType(level, arg, flag, counter)
		}
		t.Return = visitType(level, t.Return, flag, counter)
		if *counter > count {
			t.Flags |= flag
		}
		return t

	case *types.Record:
		count := *counter
		t.Row = visitType(level, t.Row, flag, counter)
		if *counter > count {
			t.Flags |= flag
		}
		return t

	case *types.Variant:
		count := *counter
		t.Row = visitType(level, t.Row, flag, counter)
		if *counter > count {
			t.Flags |= flag
		}
		return t

	case *types.RowExtend:
		count := *counter
		m := t.Labels
		mb := m.Builder()
		m.Range(func(label string, ts types.TypeList) bool {
			lb := ts.Builder()
			ts.Range(func(i int, t types.Type) bool {
				lb.Set(i, visitType(level, t, flag, counter))
				return true
			})
			mb.Set(label, lb.Build())
			return true
		})
		t.Row = visitType(level, t.Row, flag, counter)
		t.Labels = mb.Build()
		if *counter > count {
			t.Flags |= flag
		}
		return t
	}

	return t
}

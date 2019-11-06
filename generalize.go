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
	genericCount := 0
	return generalizeRecursive(level, t, &genericCount)
}

func generalizeRecursive(level int, t types.Type, genericCount *int) types.Type {
	switch t := t.(type) {
	case *types.Var:
		switch {
		case t.IsUnboundVar():
			if t.Level() > level {
				*genericCount++
				t.SetGeneric()
			}
			return t
		case t.IsLinkVar():
			return generalizeRecursive(level, t.Link(), genericCount)
		default:
			*genericCount++
			return t
		}

	case *types.App:
		gcount := *genericCount
		for i, arg := range t.Args {
			t.Args[i] = generalizeRecursive(level, arg, genericCount)
		}
		t.Const = generalizeRecursive(level, t.Const, genericCount)
		if *genericCount > gcount {
			t.HasGenericVars = true
		}
		return t

	case *types.Arrow:
		gcount := *genericCount
		for i, arg := range t.Args {
			t.Args[i] = generalizeRecursive(level, arg, genericCount)
		}
		t.Return = generalizeRecursive(level, t.Return, genericCount)
		if *genericCount > gcount {
			t.HasGenericVars = true
		}
		return t

	case *types.Record:
		gcount := *genericCount
		t.Row = generalizeRecursive(level, t.Row, genericCount)
		if *genericCount > gcount {
			t.HasGenericVars = true
		}
		return t

	case *types.Variant:
		gcount := *genericCount
		t.Row = generalizeRecursive(level, t.Row, genericCount)
		if *genericCount > gcount {
			t.HasGenericVars = true
		}
		return t

	case *types.RowExtend:
		gcount := *genericCount
		m := t.Labels
		mb := m.Builder()
		m.Range(func(label string, ts types.TypeList) bool {
			lb := ts.Builder()
			ts.Range(func(i int, t types.Type) bool {
				lb.Set(i, generalizeRecursive(level, t, genericCount))
				return true
			})
			mb.Set(label, lb.Build())
			return true
		})
		t.Row = generalizeRecursive(level, t.Row, genericCount)
		t.Labels = mb.Build()
		if *genericCount > gcount {
			t.HasGenericVars = true
		}
		return t
	}

	return t
}

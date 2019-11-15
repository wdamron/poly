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

// Generalize all unbound type-variables in t, excluding type-variables within mutable reference-types.
func Generalize(t types.Type) types.Type { return generalize(-1, t) }

// Generalize all unbound type-variables in t, including type-variables within mutable reference-types.
func GeneralizeRefs(t types.Type) types.Type { return forceGeneralize(-1, t) }

func forceGeneralize(level int, t types.Type) types.Type {
	const weak, forceGeneralize = false, true
	ngeneric, nref := 0, 0
	t = types.RealType(t)
	visitTypeVars(level, t, weak, forceGeneralize, &ngeneric, &nref)
	return t
}

func generalize(level int, t types.Type) types.Type {
	const weak, forceGeneralize = false, false
	ngeneric, nref := 0, 0
	t = types.RealType(t)
	visitTypeVars(level, t, weak, forceGeneralize, &ngeneric, &nref)
	return t
}

func visitTypeVars(level int, t types.Type, weak, forceGeneralize bool, ngeneric, nref *int) {
	switch t := t.(type) {
	case *types.Var:
		switch {
		case t.IsUnboundVar():
			if !forceGeneralize && weak {
				t.SetWeak()
			} else if t.Level() > level {
				*ngeneric++
				t.SetGeneric()
			}
		case t.IsLinkVar():
			visitTypeVars(level, t.Link(), weak, forceGeneralize, ngeneric, nref)
		case t.IsWeakVar():
		default:
			*ngeneric++
		}

	case *types.App:
		ng, nr := *ngeneric, *nref
		if types.IsRefType(t) {
			*nref++
			weak = true
		}
		for _, arg := range t.Args {
			visitTypeVars(level, arg, weak, forceGeneralize, ngeneric, nref)
		}
		t.Const = types.RealType(t.Const)
		visitTypeVars(level, t.Const, weak, forceGeneralize, ngeneric, nref)
		if t.Underlying != nil {
			t.Underlying = types.RealType(t.Underlying)
			visitTypeVars(level, t.Underlying, weak, forceGeneralize, ngeneric, nref)
		}
		if *ngeneric > ng {
			t.Flags |= types.ContainsGenericVars
		}
		if *nref > nr {
			t.Flags |= types.ContainsRefs
		}

	case *types.Arrow:
		ng, nr := *ngeneric, *nref
		for i, arg := range t.Args {
			t.Args[i] = types.RealType(arg)
			visitTypeVars(level, t.Args[i], weak, forceGeneralize, ngeneric, nref)
		}
		t.Return = types.RealType(t.Return)
		visitTypeVars(level, t.Return, weak, forceGeneralize, ngeneric, nref)
		if *ngeneric > ng {
			t.Flags |= types.ContainsGenericVars
		}
		if *nref > nr {
			t.Flags |= types.ContainsRefs
		}

	case *types.Record:
		ng, nr := *ngeneric, *nref
		t.Row = types.RealType(t.Row)
		visitTypeVars(level, t.Row, weak, forceGeneralize, ngeneric, nref)
		if *ngeneric > ng {
			t.Flags |= types.ContainsGenericVars
		}
		if *nref > nr {
			t.Flags |= types.ContainsRefs
		}

	case *types.Variant:
		ng, nr := *ngeneric, *nref
		t.Row = types.RealType(t.Row)
		visitTypeVars(level, t.Row, weak, forceGeneralize, ngeneric, nref)
		if *ngeneric > ng {
			t.Flags |= types.ContainsGenericVars
		}
		if *nref > nr {
			t.Flags |= types.ContainsRefs
		}

	case *types.RowExtend:
		ng, nr := *ngeneric, *nref
		t.Labels.Range(func(label string, ts types.TypeList) bool {
			ts.Range(func(i int, t types.Type) bool {
				visitTypeVars(level, t, weak, forceGeneralize, ngeneric, nref)
				return true
			})
			return true
		})
		t.Row = types.RealType(t.Row)
		visitTypeVars(level, t.Row, weak, forceGeneralize, ngeneric, nref)
		if *ngeneric > ng {
			t.Flags |= types.ContainsGenericVars
		}
		if *nref > nr {
			t.Flags |= types.ContainsRefs
		}
	}
}

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

package types

// Generalize all unbound type-variables in t, excluding type-variables within mutable reference-types.
func Generalize(t Type) Type { return generalizeOpts(TopLevel, t, false, false) }

// Generalize all unbound type-variables in t, marking each generalized variable as weakly-polymorphic.
func GeneralizeWeak(t Type) Type { return generalizeOpts(TopLevel, t, true, true) }

// Generalize all unbound type-variables in t, excluding type-variables within mutable reference-types.
func GeneralizeAtLevel(level int, t Type) Type { return generalizeOpts(level, t, false, true) }

// Generalize all unbound type-variables in t, including type-variables within mutable reference-types.
func GeneralizeRefs(t Type) Type { return generalizeOpts(TopLevel, t, false, true) }

// Mark all type-variables in t as weakly-polymorphic. Weakly-polymorphic types will not be generalized during inference.
func MarkWeak(t Type) Type { return generalizeOpts(TopLevel, t, true, false) }

func generalizeOpts(level int, t Type, weak, forceGeneralize bool) Type {
	ngeneric, nref := 0, 0
	t = RealType(t)
	visitTypeVars(level, t, weak, forceGeneralize, &ngeneric, &nref)
	return t
}

func visitTypeVars(level int, t Type, weak, forceGeneralize bool, ngeneric, nref *int) {
	switch t := t.(type) {
	case *Var:
		switch {
		case t.IsLinkVar():
			visitTypeVars(level, t.Link(), weak, forceGeneralize, ngeneric, nref)
		case t.IsGenericVar():
			*ngeneric++
			// Weak type-variables may not be re-generalized after instantiation:
			if weak {
				t.SetWeak()
			}
		default: // weak or unbound
			// See "Efficient Generalization with Levels" (Oleg Kiselyov) -- http://okmij.org/ftp/ML/generalization.html#levels
			//
			// If the current level is less than the type-variable's level, a let-binding where the type-variable was instantiated
			// is being generalized:
			if t.LevelNum() > level && (forceGeneralize || (!weak && !t.IsWeakVar())) {
				*ngeneric++
				t.SetGeneric()
			}
			// Weak type-variables may not be re-generalized after instantiation:
			if weak {
				t.SetWeak()
			}
		}

	case *RecursiveLink:
		ng, nr := *ngeneric, *nref
		weak, rec := true, t.Recursive
		if rec.Generalized || rec.Instantiated {
			return
		}
		rec.Generalized = true
		for _, ti := range rec.Types {
			visitTypeVars(level, ti, weak, forceGeneralize, ngeneric, nref)
		}
		if *ngeneric > ng {
			rec.Flags |= ContainsGenericVars
		}
		if *nref > nr {
			rec.Flags |= ContainsRefs
		}
		return

	case *App:
		ng, nr := *ngeneric, *nref
		if IsRefType(t) {
			*nref++
			weak = true
			t.Flags |= WeaklyPolymorphic
		} else if weak {
			t.Flags |= WeaklyPolymorphic
		}
		for i, arg := range t.Args {
			t.Args[i] = RealType(arg)
			visitTypeVars(level, t.Args[i], weak, forceGeneralize, ngeneric, nref)
		}
		t.Const = RealType(t.Const)
		visitTypeVars(level, t.Const, weak, forceGeneralize, ngeneric, nref)
		if t.Underlying != nil {
			t.Underlying = RealType(t.Underlying)
			visitTypeVars(level, t.Underlying, weak, forceGeneralize, ngeneric, nref)
		}
		if *ngeneric > ng {
			t.Flags |= ContainsGenericVars
		}
		if *nref > nr {
			t.Flags |= ContainsRefs
		}

	case *Arrow:
		ng, nr := *ngeneric, *nref
		for i, arg := range t.Args {
			t.Args[i] = RealType(arg)
			visitTypeVars(level, t.Args[i], weak, forceGeneralize, ngeneric, nref)
		}
		t.Return = RealType(t.Return)
		visitTypeVars(level, t.Return, weak, forceGeneralize, ngeneric, nref)
		if *ngeneric > ng {
			t.Flags |= ContainsGenericVars
		}
		if *nref > nr {
			t.Flags |= ContainsRefs
		}

	case *Record:
		ng, nr := *ngeneric, *nref
		t.Row = RealType(t.Row)
		visitTypeVars(level, t.Row, weak, forceGeneralize, ngeneric, nref)
		if *ngeneric > ng {
			t.Flags |= ContainsGenericVars
		}
		if *nref > nr {
			t.Flags |= ContainsRefs
		}

	case *Variant:
		ng, nr := *ngeneric, *nref
		t.Row = RealType(t.Row)
		visitTypeVars(level, t.Row, weak, forceGeneralize, ngeneric, nref)
		if *ngeneric > ng {
			t.Flags |= ContainsGenericVars
		}
		if *nref > nr {
			t.Flags |= ContainsRefs
		}

	case *RowExtend:
		ng, nr := *ngeneric, *nref
		t.Labels.Range(func(label string, ts TypeList) bool {
			ts.Range(func(i int, t Type) bool {
				visitTypeVars(level, RealType(t), weak, forceGeneralize, ngeneric, nref)
				return true
			})
			return true
		})
		t.Row = RealType(t.Row)
		visitTypeVars(level, t.Row, weak, forceGeneralize, ngeneric, nref)
		if *ngeneric > ng {
			t.Flags |= ContainsGenericVars
		}
		if *nref > nr {
			t.Flags |= ContainsRefs
		}
	}
}

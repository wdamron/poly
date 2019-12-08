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
	"github.com/wdamron/poly/types"
)

func GeneralizeOpts(level uint, t types.Type, forceGeneralize, weak bool) types.Type {
	// Path compression:
	t = types.RealType(t)
	visitTypeVars(level, t, forceGeneralize, weak)
	return t
}

func visitTypeVars(level uint, t types.Type, forceGeneralize, weak bool) (tf types.TypeFlags) {
	switch t := t.(type) {
	case *types.Unit:
		return

	case *types.Var:
		switch {
		case t.IsLinkVar():
			return visitTypeVars(level, t.Link(), forceGeneralize, weak)
		case t.IsGenericVar():
			tf |= types.ContainsGenericVars
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
				tf |= types.ContainsGenericVars
				t.SetGeneric()
			}
			// Weak type-variables may not be re-generalized after instantiation:
			if weak {
				t.SetWeak()
			}
		}

	case *types.RecursiveLink:
		rec := t.Recursive
		if !rec.NeedsGeneralization() { // break cycles
			if rec.IsGeneric() {
				tf |= types.ContainsGenericVars
			}
			if rec.HasRefs() {
				tf |= types.ContainsRefs
			}
			return
		}
		rec.Flags &^= types.NeedsGeneralization // break cycles
		for _, alias := range rec.Types {
			tf |= visitTypeVars(level, alias, forceGeneralize, weak)
		}
		rec.Flags |= tf
		// back-propagate type-flags through links:
		if tf&(types.ContainsGenericVars|types.ContainsRefs) != 0 {
			for _, alias := range rec.Types {
				visitTypeVars(level, alias, forceGeneralize, weak)
			}
		}

	case *types.App:
		if types.IsRefType(t) {
			tf |= types.ContainsRefs
			weak = true
		}
		for i, param := range t.Params {
			t.Params[i] = types.RealType(param)
			tf |= visitTypeVars(level, t.Params[i], forceGeneralize, weak)
		}
		t.Const = types.RealType(t.Const)
		tf |= visitTypeVars(level, t.Const, forceGeneralize, weak)
		if t.Underlying != nil {
			t.Underlying = types.RealType(t.Underlying)
			tf |= visitTypeVars(level, t.Underlying, forceGeneralize, weak)
		}
		t.Flags |= tf

	case *types.Arrow:
		for i, arg := range t.Args {
			t.Args[i] = types.RealType(arg)
			tf |= visitTypeVars(level, t.Args[i], forceGeneralize, weak)
		}
		t.Return = types.RealType(t.Return)
		tf |= visitTypeVars(level, t.Return, forceGeneralize, weak)
		t.Flags |= tf

	case *types.Record:
		t.Row = types.RealType(t.Row)
		tf |= visitTypeVars(level, t.Row, forceGeneralize, weak)
		t.Flags |= tf

	case *types.Variant:
		t.Row = types.RealType(t.Row)
		tf |= visitTypeVars(level, t.Row, forceGeneralize, weak)
		t.Flags |= tf

	case *types.RowExtend:
		t.Labels.Range(func(label string, ts types.TypeList) bool {
			ts.Range(func(i int, t types.Type) bool {
				tf |= visitTypeVars(level, types.RealType(t), forceGeneralize, weak)
				return true
			})
			return true
		})
		t.Row = types.RealType(t.Row)
		tf |= visitTypeVars(level, t.Row, forceGeneralize, weak)
		t.Flags |= tf
	}
	return
}

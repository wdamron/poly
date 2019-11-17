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

import (
	"errors"
)

// Type is the base for all types.
type Type interface {
	TypeName() string
	// Check if a type is a generic type-variable or contains generic type-variables.
	IsGeneric() bool
	// Check if a type is a mutable reference-type or contains mutable reference-types.
	HasRefs() bool
}

type TypeFlags uint

const (
	// TypeFlags for a type which is a generic type-variable or contains generic type-variables
	ContainsGenericVars TypeFlags = 1
	// TypeFlags for a type which is a mutable reference-type or contains mutable reference-types
	ContainsRefs TypeFlags = 2
	// Weakly-polymorphic types may not be generalized.
	WeaklyPolymorphic = 4
)

// Mutable references are applications of RefType (a mutable reference-type) with a single referenced type-parameter.
var RefType = &Const{"ref"}

// Check if a type application is a mutable reference-type.
func IsRefType(app *App) bool {
	c, _ := app.Const.(*Const)
	return c == RefType
}

var (
	_ Type = (*Var)(nil)
	_ Type = (*Const)(nil)
	_ Type = (*App)(nil)
	_ Type = (*Arrow)(nil)
	_ Type = (*Method)(nil)
	_ Type = (*Record)(nil)
	_ Type = (*Variant)(nil)
	_ Type = (*RowExtend)(nil)
	_ Type = (*RowEmpty)(nil)
)

// Create an application of RefType (a mutable reference-type) with a single referenced type-parameter.
func NewRef(deref Type) *App { return &App{Const: RefType, Args: []Type{deref}} }

// "Var"
func (t *Var) TypeName() string { return "Var" }

// Name of the type-constant
func (t *Const) TypeName() string { return t.Name }

// "App"
func (t *App) TypeName() string { return "App" }

// "Arrow"
func (t *Arrow) TypeName() string { return "Arrow" }

// "Method"
func (t *Method) TypeName() string { return "Method" }

// "Record"
func (t *Record) TypeName() string { return "Record" }

// "Variant"
func (t *Variant) TypeName() string { return "Variant" }

// "RowExtend"
func (t *RowExtend) TypeName() string { return "RowExtend" }

// "RowEmpty"
func (t *RowEmpty) TypeName() string { return "RowEmpty" }

// Check if t is a generic type-variable.
func (t *Var) IsGeneric() bool {
	r := RealType(t)
	if tv, ok := r.(*Var); ok {
		return tv.IsGenericVar()
	}
	return r.IsGeneric()
}

// Check if t is linked to a type containing mutable reference-types.
func (t *Var) HasRefs() bool {
	r := RealType(t)
	if _, ok := r.(*Var); ok {
		return false
	}
	return r.HasRefs()
}

// Const is never generic.
func (t *Const) IsGeneric() bool { return false }

// Const never contains mutable reference-types.
func (t *Const) HasRefs() bool { return false }

// Check if t contains generic types.
func (t *App) IsGeneric() bool { return t.Flags&ContainsGenericVars != 0 }

// Check if t contains mutable reference-types.
func (t *App) HasRefs() bool { return t.Flags&ContainsRefs != 0 || IsRefType(t) }

// Check if t is weakly-polymorphic and cannot be generalized.
func (t *App) IsWeak() bool { return t.Flags&WeaklyPolymorphic != 0 || t.HasRefs() }

// Check if t contains generic types.
func (t *Arrow) IsGeneric() bool { return t.Flags&ContainsGenericVars != 0 }

// Check if t contains mutable reference-types.
func (t *Arrow) HasRefs() bool { return t.Flags&ContainsRefs != 0 }

// Check if t contains generic types.
func (t *Method) IsGeneric() bool { return t.TypeClass.Methods[t.Name].Flags&ContainsGenericVars != 0 }

// Check if t contains mutable reference-types.
func (t *Method) HasRefs() bool { return t.TypeClass.Methods[t.Name].Flags&ContainsRefs != 0 }

// Check if t contains generic types.
func (t *Record) IsGeneric() bool { return t.Flags&ContainsGenericVars != 0 }

// Check if t contains mutable reference-types.
func (t *Record) HasRefs() bool { return t.Flags&ContainsRefs != 0 }

// Check if t contains generic types.
func (t *Variant) IsGeneric() bool { return t.Flags&ContainsGenericVars != 0 }

// Check if t contains mutable reference-types.
func (t *Variant) HasRefs() bool { return t.Flags&ContainsRefs != 0 }

// Check if t contains generic types.
func (t *RowExtend) IsGeneric() bool { return t.Flags&ContainsGenericVars != 0 }

// Check if t contains mutable reference-types.
func (t *RowExtend) HasRefs() bool { return t.Flags&ContainsRefs != 0 }

// RowEmpty is never generic.
func (t *RowEmpty) IsGeneric() bool { return false }

// RowEmpty never contains mutable reference-types.
func (t *RowEmpty) HasRefs() bool { return false }

// Type constant: `int`, `bool`, etc
type Const struct {
	Name string
}

// Type application: `list[int]`
type App struct {
	Const Type
	Args  []Type
	// Aliased type (optional)
	Underlying Type
	Flags      TypeFlags
}

// Function type: `(int, int) -> int`
type Arrow struct {
	Args   []Type
	Return Type
	// Method which the function instantiates, or nil
	Method *Method
	Flags  TypeFlags
}

// Type-class method type: `('a, int) -> 'a`
type Method struct {
	TypeClass *TypeClass
	Name      string
	Flags     TypeFlags
}

// Record type: `{a : int}`
type Record struct {
	Row   Type
	Flags TypeFlags
}

// Tagged (ad-hoc) variant-type: `[i : int, s : string]`
type Variant struct {
	Row   Type
	Flags TypeFlags
}

// Row extension: `<a : _ , b : _ | ...>`
type RowExtend struct {
	Row    Type
	Labels TypeMap
	Flags  TypeFlags
}

var RowEmptyPointer = (*RowEmpty)(nil)

// Empty row: `<>`
type RowEmpty struct{}

// Get the underlying type for a chain of linked type-variables, when applicable.
func RealType(t Type) Type {
	for {
		tv, ok := t.(*Var)
		if !ok {
			return t
		}
		if !tv.IsLinkVar() {
			return t
		}
		t = tv.Link()
	}
	panic("unreachable")
}

// Flatten row extensions into a single row.
func FlattenRowType(t Type) (labels TypeMap, row Type, err error) {
	lb := NewTypeMapBuilder()
	row, err = flattenRowType(lb, t)
	if err == nil {
		labels = lb.Build()
	}
	return
}

func flattenRowType(labels TypeMapBuilder, t Type) (Type, error) {
	switch t := t.(type) {
	case *RowExtend:
		restType, err := flattenRowType(labels, t.Row)
		if err != nil {
			return t, err
		}
		labels.Merge(t.Labels)
		return restType, nil
	case *Var:
		if t.IsLinkVar() {
			return flattenRowType(labels, t.Link())
		}
		return t, nil
	case *RowEmpty:
		return t, nil
	default:
		return t, errors.New("Not a row type")
	}
}

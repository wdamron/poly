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
	IsGeneric() bool
}

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

// Const is never generic.
func (t *Const) IsGeneric() bool { return false }

// Check if t contains any generic types.
func (t *App) IsGeneric() bool { return t.HasGenericVars }

// Check if t contains any generic types.
func (t *Arrow) IsGeneric() bool { return t.HasGenericVars }

// Check if t contains any generic types.
func (t *Method) IsGeneric() bool { return t.TypeClass.Methods[t.Name].HasGenericVars }

// Check if t contains any generic types.
func (t *Record) IsGeneric() bool { return t.HasGenericVars }

// Check if t contains any generic types.
func (t *Variant) IsGeneric() bool { return t.HasGenericVars }

// Check if t contains any generic types.
func (t *RowExtend) IsGeneric() bool { return t.HasGenericVars }

// RowEmpty is never generic.
func (t *RowEmpty) IsGeneric() bool { return false }

// Type constant: `int` or `bool`
type Const struct {
	Name string
}

// Type application: `list[int]`
type App struct {
	Const          Type
	Args           []Type
	HasGenericVars bool
}

// Function type: `(int, int) -> int`
type Arrow struct {
	Args   []Type
	Return Type
	// Method which the function instantiates, or nil
	Method         *Method
	HasGenericVars bool
}

// Type-class method type: `('a, int) -> 'a`
type Method struct {
	TypeClass      *TypeClass
	Name           string
	HasGenericVars bool
}

// Record type: `{...}`
type Record struct {
	Row            Type
	HasGenericVars bool
}

// Variant type: `[...]`
type Variant struct {
	Row            Type
	HasGenericVars bool
}

// Row extension: `<a : _ , b : _ | ...>`
type RowExtend struct {
	Row            Type
	Labels         TypeMap
	HasGenericVars bool
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

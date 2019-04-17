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

const (
	GenericVarLevel = 1<<31 - 1
	LinkVarLevel    = -1 << 31
)

// Type is the base interface for all types.
type Type interface {
	TypeName() string
	IsGeneric() bool
}

func (t *Var) TypeName() string       { return "Var" }
func (t *Const) TypeName() string     { return "Const" }
func (t *App) TypeName() string       { return "App" }
func (t *Arrow) TypeName() string     { return "Arrow" }
func (t *Record) TypeName() string    { return "Record" }
func (t *Variant) TypeName() string   { return "Variant" }
func (t *RowExtend) TypeName() string { return "RowExtend" }
func (t RowEmpty) TypeName() string   { return "RowEmpty" }

func (t *Var) IsGeneric() bool {
	r := RealType(t)
	if tv, ok := r.(*Var); ok {
		return tv.IsGenericVar()
	}
	return r.IsGeneric()
}

func (t *Const) IsGeneric() bool     { return false }
func (t *App) IsGeneric() bool       { return t.HasGenericVars }
func (t *Arrow) IsGeneric() bool     { return t.HasGenericVars }
func (t *Record) IsGeneric() bool    { return t.HasGenericVars }
func (t *Variant) IsGeneric() bool   { return t.HasGenericVars }
func (t *RowExtend) IsGeneric() bool { return t.HasGenericVars }
func (t RowEmpty) IsGeneric() bool   { return false }

// Type variable
type Var struct {
	link  Type
	id    int32
	level int32
}

// Instance of a type variable
type VarType int

const (
	UnboundVar VarType = iota
	LinkVar
	GenericVar
)

func NewVar(id, level int) *Var {
	return &Var{id: int32(id), level: int32(level)}
}

func NewGenericVar(id int) *Var {
	return &Var{id: int32(id), level: GenericVarLevel}
}

func (tv *Var) VarType() VarType {
	switch tv.level {
	case LinkVarLevel:
		return LinkVar
	case GenericVarLevel:
		return GenericVar
	default:
		return UnboundVar
	}
}

func (tv *Var) Link() Type { return tv.link }
func (tv *Var) Id() int    { return int(tv.id) }
func (tv *Var) Level() int { return int(tv.level) }

func (tv *Var) IsUnboundVar() bool { return tv.level != LinkVarLevel && tv.level != GenericVarLevel }
func (tv *Var) IsLinkVar() bool    { return tv.level == LinkVarLevel }
func (tv *Var) IsGenericVar() bool { return tv.level == GenericVarLevel }

func (tv *Var) SetId(id int)   { tv.id = int32(id) }
func (tv *Var) SetLink(t Type) { tv.link, tv.id, tv.level = t, -1, LinkVarLevel }
func (tv *Var) SetGeneric()    { tv.link, tv.level = nil, GenericVarLevel }
func (tv *Var) AdjustLevel(level int) {
	if level == GenericVarLevel {
		tv.SetGeneric()
		return
	}
	tv.link, tv.level = nil, int32(level)
}
func (tv *Var) Update(id, level int, link Type) {
	tv.link, tv.id, tv.level = link, int32(id), int32(level)
}

// Type constant: `int` or `bool`
type Const struct {
	Name string
}

// Type application: `list[int]`
type App struct {
	Func           Type
	Args           []Type
	HasGenericVars bool
}

// Function type: `(int, int) -> int`
type Arrow struct {
	Args           []Type
	Return         Type
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
	case RowEmpty:
		return t, nil
	default:
		return t, errors.New("Not a row type")
	}
}

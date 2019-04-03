package types

import (
	"errors"
)

type Type interface {
	TypeName() string
}

func (t *Const) TypeName() string     { return "Const" }
func (t *Var) TypeName() string       { return "Var" }
func (t *App) TypeName() string       { return "App" }
func (t *Arrow) TypeName() string     { return "Arrow" }
func (t *Record) TypeName() string    { return "Record" }
func (t *Variant) TypeName() string   { return "Variant" }
func (t *RowExtend) TypeName() string { return "RowExtend" }
func (t RowEmpty) TypeName() string   { return "RowEmpty" }

// Type constant: `int` or `bool`
type Const struct {
	Name string
}

// Type variable
type Var struct {
	Instance VarType
}

func (tvar *Var) Set(instance VarType) { tvar.Instance = instance }

// Type-variable instance
type VarType interface {
	VarTypeName() string
}

func (v UnboundVar) VarTypeName() string { return "UnboundVar" }
func (v LinkVar) VarTypeName() string    { return "LinkVar" }
func (v GenericVar) VarTypeName() string { return "GenericVar" }

// Unbound type-variable
type UnboundVar struct {
	Id    int
	Level int
}

// Link type-variable
type LinkVar struct {
	Type Type
}

// Generic type-variable
type GenericVar struct {
	Id int
}

// Type application: `list[int]`
type App struct {
	Func Type
	Args []Type
}

// Function type: `(int, int) -> int`
type Arrow struct {
	Args   []Type
	Return Type
}

// Record type: `{...}`
type Record struct {
	Row Type
}

// Variant type: `[...]`
type Variant struct {
	Row Type
}

// Row extension: `<a : _ , b : _ | ...>`
type RowExtend struct {
	Row    Type
	Labels TypeMap
}

// Empty row: `<>`
type RowEmpty struct{}

func RealType(t Type) Type {
	tv, ok := t.(*Var)
	if !ok {
		return t
	}
	ln, ok := tv.Instance.(LinkVar)
	if !ok {
		return t
	}
	return RealType(ln.Type)
}

func MatchRowType(t Type) (TypeMap, Type, error) {
	labels := NewTypeMapBuilder()
	row, err := flattenRowType(labels, t)
	return labels.Build(), row, err
}

func flattenRowType(labels TypeMapBuilder, t Type) (Type, error) {
	switch tt := t.(type) {
	case *RowExtend:
		restType, err := flattenRowType(labels, tt.Row)
		if err != nil {
			return tt, err
		}
		labels.Merge(tt.Labels)
		return restType, nil
	case *Var:
		if ln, ok := tt.Instance.(LinkVar); ok {
			return flattenRowType(labels, ln.Type)
		}
		return tt, nil
	case RowEmpty:
		return tt, nil
	default:
		return tt, errors.New("Not a row type")
	}
}

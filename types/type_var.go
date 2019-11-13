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

// Special binding-levels (used as flags):
const (
	GenericVarLevel = -1 << 31
	LinkVarLevel    = (-1 << 31) + 1
	// Type-variables within mutable reference-types may not be generalized
	WeakVarLevel = (-1 << 31) + 2
)

// Type-variable
type Var struct {
	constraints []InstanceConstraint
	link        Type
	id          int32
	level       int32
}

// Instance of a type-variable
type VarType int

const (
	// Unbound type-variable
	UnboundVar VarType = iota
	// Linked type-variable
	LinkVar
	// Generic type-variable
	GenericVar
	// Weak type-variable (within a mutable reference-type)
	WeakVar
)

// Create a new type-variable with the given id and binding-level.
func NewVar(id, level int) *Var {
	return &Var{id: int32(id), level: int32(level)}
}

// Create a new generic type-variable.
func NewGenericVar(id int) *Var {
	return &Var{id: int32(id), level: GenericVarLevel}
}

// VarType indicates whether the type-variable is linked, unbound, or generic.
func (tv *Var) VarType() VarType {
	switch tv.level {
	case LinkVarLevel:
		return LinkVar
	case GenericVarLevel:
		return GenericVar
	case WeakVarLevel:
		return WeakVar
	default:
		return UnboundVar
	}
}

// Constraints returns the set of type-classes which the type-variable must implement.
func (tv *Var) Constraints() []InstanceConstraint { return tv.constraints }

// Id returns the unique identifier of the type-variable.
func (tv *Var) Id() int { return int(tv.id) }

// Level returns the adjusted binding-level of the type-variable.
func (tv *Var) Level() int { return int(tv.level) }

// Link returns the type which the type-variable is bound to, if the type-variable is bound.
func (tv *Var) Link() Type { return tv.link }

func (tv *Var) IsUnboundVar() bool { return tv.level > LinkVarLevel }
func (tv *Var) IsLinkVar() bool    { return tv.level == LinkVarLevel }
func (tv *Var) IsGenericVar() bool { return tv.level == GenericVarLevel }
func (tv *Var) IsWeakVar() bool    { return tv.level == WeakVarLevel }

// Set the binding-level of the type-variable to the generic level.
func (tv *Var) SetGeneric() { tv.level = GenericVarLevel }

// Set the binding-level of the type-variable to the weak level. Weak type-variables may not be generalized.
func (tv *Var) SetWeak() { tv.level = WeakVarLevel }

// Set the unique identifier of the type-variable.
func (tv *Var) SetId(id int) { tv.id = int32(id) }

// Set the adjusted binding-level of the type-variable.
func (tv *Var) SetLevel(level int) { tv.level = int32(level) }

// Set the type which the type-variable is bound to. If the type-variable has a qualified
// type, its predicates will be checked against the linked type.
func (tv *Var) SetLink(t Type) { tv.link, tv.level = t, LinkVarLevel }

// Constrain the type-variable to types which implement a set of type-classes.
func (tv *Var) SetConstraints(constraints []InstanceConstraint) { tv.constraints = constraints }

// Constrain the type-variable to types which implement a type-class.
func (tv *Var) AddConstraint(tc *TypeClass) {
	tv.constraints = append(tv.constraints, InstanceConstraint{TypeClass: tc})
}

// Flatten a chain of linked type-variables. Predicates for type-variables with qualified types
// will not be checked during flattening.
func (tv *Var) Flatten() {
	if tv.IsLinkVar() {
		tv.link = RealType(tv.link)
	}
}

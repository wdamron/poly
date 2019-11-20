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

// Special binding-levels for type-variables:
const (
	TopLevel        = 0
	GenericVarLevel = 1 << 31
	LinkVarLevel    = 1 << 30
	// Type-variables within mutable reference-types are weakly-polymorphic and may not be generalized
	WeakVarLevel = 1 << 29

	// Restricted levels (0x01...0x1f) << 24:
	SizeVarLevel = 1 << 24

	RestrictedLevelMask = 0x1f << 24
)

var _ Type = (*Var)(nil)

// Type-variable
type Var struct {
	constraints []InstanceConstraint
	link        Type
	id          int32
	level       uint32
}

// Create a new type-variable with the given id and binding-level.
func NewVar(id, level int) *Var {
	return &Var{id: int32(id), level: uint32(level)}
}

// Create a new generic type-variable.
func NewGenericVar(id int) *Var {
	return &Var{id: int32(id), level: GenericVarLevel}
}

// Constraints returns the set of type-classes which the type-variable must implement.
func (tv *Var) Constraints() []InstanceConstraint { return tv.constraints }

// Id returns the unique identifier of the type-variable.
func (tv *Var) Id() int { return int(tv.id) }

// Level returns the adjusted binding-level of the type-variable, with flag levels included.
func (tv *Var) Level() int { return int(tv.level) }

// Level returns the adjusted binding-level of the type-variable, with flag levels excluded.
func (tv *Var) LevelNum() int { return int(tv.levelNum()) }

func (tv *Var) levelNum() uint32 { return tv.level &^ (0xff << 24) }

// Link returns the type which the type-variable is bound to, if the type-variable is bound.
func (tv *Var) Link() Type { return tv.link }

func (tv *Var) IsUnboundVar() bool    { return tv.level&(GenericVarLevel|LinkVarLevel) == 0 }
func (tv *Var) IsLinkVar() bool       { return tv.level&LinkVarLevel != 0 }
func (tv *Var) IsGenericVar() bool    { return tv.level&GenericVarLevel != 0 }
func (tv *Var) IsWeakVar() bool       { return tv.level&WeakVarLevel != 0 }
func (tv *Var) IsSizeVar() bool       { return tv.level&RestrictedLevelMask == SizeVarLevel }
func (tv *Var) IsRestrictedVar() bool { return tv.level&RestrictedLevelMask != 0 }

// Set the binding-level of the type-variable to the generic level.
func (tv *Var) SetGeneric() {
	tv.level = tv.levelNum() | (tv.level & (RestrictedLevelMask | WeakVarLevel)) | GenericVarLevel
}

// Set the binding-level of the type-variable to the weak level. Weak type-variables may not be generalized.
func (tv *Var) SetWeak() {
	tv.level = tv.levelNum() | (tv.level & (RestrictedLevelMask | GenericVarLevel)) | WeakVarLevel
}

// Set the unique identifier of the type-variable.
func (tv *Var) SetId(id int) { tv.id = int32(id) }

// Set the adjusted binding-level of the type-variable, with existing flags retained.
func (tv *Var) SetLevelNum(level int) {
	tv.level = (uint32(level) &^ (0xff << 24)) | (tv.level & 0xff << 24)
}

// Set the type which the type-variable is bound to.
func (tv *Var) SetLink(t Type) {
	tv.link = t
	tv.level = tv.levelNum() | (tv.level & RestrictedLevelMask) | LinkVarLevel
}

// Restrict t as a size type-variable. Size type-variables may only unify with size types.
func (tv *Var) RestrictSizeVar() {
	tv.level = (tv.level &^ RestrictedLevelMask) | SizeVarLevel
}

// Constrain the type-variable to types which implement a set of type-classes.
func (tv *Var) SetConstraints(constraints []InstanceConstraint) { tv.constraints = constraints }

// Constrain the type-variable to types which implement a type-class.
func (tv *Var) AddConstraint(constraint InstanceConstraint) {
	for i, existing := range tv.constraints {
		if existing.TypeClass.Id == constraint.TypeClass.Id || existing.TypeClass.HasSuperClass(constraint.TypeClass) {
			return
		}
		if constraint.TypeClass.HasSuperClass(existing.TypeClass) {
			tv.constraints[i] = constraint
			return
		}
	}
	tv.constraints = append(tv.constraints, constraint)
}

// Flatten a chain of linked type-variables. Predicates for type-variables with qualified types
// will not be checked during flattening.
func (tv *Var) Flatten() {
	if tv.IsLinkVar() {
		tv.link = RealType(tv.link)
	}
}

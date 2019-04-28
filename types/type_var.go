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
	GenericVarLevel = 1<<31 - 1
	LinkVarLevel    = -1 << 31
)

// Type-variable
type Var struct {
	kinds []*Kind
	link  Type
	id    int32
	level int32
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
	default:
		return UnboundVar
	}
}

// Id returns the unique identifier of the type-variable.
func (tv *Var) Id() int { return int(tv.id) }

// Level returns the adjusted binding-level of the type-variable.
func (tv *Var) Level() int { return int(tv.level) }

// Link returns the type which the type-variable is bound to, if the type-variable is bound.
func (tv *Var) Link() Type { return tv.link }

// Kinds returns qualifications applied to the type-variable.
func (tv *Var) Kinds() []*Kind { return tv.kinds }

func (tv *Var) IsUnboundVar() bool { return tv.level != LinkVarLevel && tv.level != GenericVarLevel }
func (tv *Var) IsLinkVar() bool    { return tv.level == LinkVarLevel }
func (tv *Var) IsGenericVar() bool { return tv.level == GenericVarLevel }

// Set the unique identifier of the type-variable.
func (tv *Var) SetId(id int) { tv.id = int32(id) }

// Set the adjusted binding-level of the type-variable.
func (tv *Var) SetLevel(level int) { tv.level = int32(level) }

// Set the type which the type-variable is bound to. If the type-variable has a qualified
// type, the refinements for its kinds will be applied to it.
func (tv *Var) SetLink(t Type) error {
	tv.link, tv.level = t, LinkVarLevel
	for _, k := range tv.kinds {
		if err := k.Refine(tv, k); err != nil {
			return err
		}
	}
	return nil
}

// Flatten a chain of linked type-variables. If the type-variable has a qualified type,
// the refinements for its kinds will not be applied to it during flattening.
func (tv *Var) Flatten() {
	if tv.IsLinkVar() {
		tv.link = RealType(tv.link)
	}
}

// Set the binding-level of the type-variable to the generic level.
func (tv *Var) SetGeneric() { tv.level = GenericVarLevel }

// Add a kind to the type-variable's qualified type.
func (tv *Var) AddKind(k *Kind) {
	for _, existing := range tv.kinds {
		if existing.Id == k.Id {
			return
		}
	}
	tv.kinds = append(tv.kinds, k)
}

// Add a set of kinds to the type-variable's qualified type.
func (tv *Var) AddKinds(ks []*Kind) {
	for _, k := range ks {
		tv.AddKind(k)
	}
}

// Remove a kind from the type-variable's qualified type.
func (tv *Var) RemoveKind(k *Kind) {
	switch len(tv.kinds) {
	case 0:
		return
	case 1:
		if tv.kinds[0].Id == k.Id {
			tv.kinds = nil
		}
	default:
		for i, existing := range tv.kinds {
			if existing.Id != k.Id {
				continue
			}
			if i < len(tv.kinds)-1 {
				copy(tv.kinds[i:], tv.kinds[i+1:])
			}
			tv.kinds = tv.kinds[:len(tv.kinds)-1]
			return
		}
	}
}

// Remove a set of kinds from the type-variable's qualified type.
func (tv *Var) RemoveKinds(ks []*Kind) {
	for _, k := range ks {
		tv.RemoveKind(k)
	}
}

// Move a kind from the type-variable's qualified type to another type-variable's qualified type.
func (tv *Var) MoveKind(to *Var, k *Kind) {
	if tv == to {
		return
	}
	tv.RemoveKind(k)
	to.AddKind(k)
}

// Move a set of kinds from the type-variable's qualified type to another type-variable's qualified type.
func (tv *Var) MoveKinds(to *Var, ks []*Kind) {
	for _, k := range ks {
		tv.MoveKind(to, k)
	}
}

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
	"github.com/wdamron/poly/internal/util"
)

// MethodSet is a set of named function-types declared for a type-class or instance.
type MethodSet map[string]*Arrow

// Parameterized type-class
type TypeClass struct {
	// Id should uniquely identify the type-class
	Id uint
	// Name should uniquely identify the type-class
	Name      string
	Param     Type
	Methods   MethodSet
	Super     map[uint]*TypeClass
	Sub       map[uint]*TypeClass
	Instances []*Instance
	// Union type-classes may be cast to a tagged (ad-hoc) variant from their labelled instances
	Union        map[string]*Instance
	UnionVariant *Variant

	// partially-grouped instances for faster lookups:

	tconst    map[string]*Instance   // mapped by name
	tappconst map[string][]*Instance // grouped by constructor name
	trecord   []*Instance
	tvariant  []*Instance
	tmisc     []*Instance // instances not in the above groups
}

// Instance of a parameterized type-class
type Instance struct {
	TypeClass *TypeClass
	Param     Type
	Methods   MethodSet
	// Strict disables type-variable unification during instance matching.
	Strict bool
	// MethodNames maps method names to names of their implementations within the type-environment.
	MethodNames map[string]string
}

func (inst *Instance) SetStrict(strict bool) { inst.Strict = strict }

// InstanceConstraint constrains a type-variable to types which implement a type-class.
type InstanceConstraint struct {
	TypeClass *TypeClass
}

// Create a new named/parameterized type-class with a set of method declarations.
func NewTypeClass(id uint, name string, param Type, methods MethodSet) *TypeClass {
	return &TypeClass{Id: id, Name: name, Param: param, Methods: methods}
}

// Add a super-class to the type-class. This is an alias for `super.AddSubClass(sub)`.
func (sub *TypeClass) AddSuperClass(super *TypeClass) { super.AddSubClass(sub) }

// Add a sub-class to the type-class.
func (super *TypeClass) AddSubClass(sub *TypeClass) {
	if super.Sub != nil && super.Sub[sub.Id] != nil {
		return
	}
	if sub.Super == nil {
		sub.Super = make(map[uint]*TypeClass)
	}
	if super.Sub == nil {
		super.Sub = make(map[uint]*TypeClass)
	}
	sub.Super[super.Id] = super
	super.Sub[sub.Id] = sub
}

// Add an instance to the type-class with param as the type-parameter.
//
// methodNames must map from method names to names of their implementations within the type-environment.
func (tc *TypeClass) AddInstance(param Type, methods MethodSet, methodNames map[string]string) *Instance {
	inst := &Instance{TypeClass: tc, Param: param, Methods: methods, MethodNames: methodNames}
	switch param := param.(type) {
	case *Const:
		if tc.tconst == nil {
			tc.tconst = make(map[string]*Instance)
		}
		tc.tconst[param.Name] = inst
	case *App:
		if c, ok := param.Const.(*Const); ok {
			if tc.tappconst == nil {
				tc.tappconst = make(map[string][]*Instance)
			}
			tc.tappconst[c.Name] = append(tc.tappconst[c.Name], inst)
			break
		}
		tc.tmisc = append(tc.tmisc, inst)
	case *Record:
		tc.trecord = append(tc.trecord, inst)
	case *Variant:
		tc.tvariant = append(tc.tvariant, inst)
	default:
		tc.tmisc = append(tc.tmisc, inst)
	}
	tc.Instances = append(tc.Instances, inst)
	return inst
}

// Check if a type-class is declared as a sub-class of another type-class.
func (tc *TypeClass) HasSuperClass(super *TypeClass) bool {
	seen := util.NewUintDedupeMap()
	found := tc.hasSuperClass(seen, super.Id)
	seen.Release()
	return found
}

func (tc *TypeClass) hasSuperClass(seen util.UintDedupeMap, id uint) bool {
	seen[tc.Id] = true
	for superId, super := range tc.Super {
		switch {
		case seen[superId]:
			return false
		case superId == id, super.hasSuperClass(seen, id):
			return true
		}
	}
	return false
}

// Visit all instances for the type-class and all sub-classes. Sub-classes will be visited first.
func (tc *TypeClass) FindInstance(found func(*Instance) bool) bool {
	seen := util.NewUintDedupeMap()
	ok, _ := tc.findInstance(seen, found)
	seen.Release()
	return ok
}

// Visit all instances for the type-class and all sub-classes, filtered by a type-parameter. Sub-classes will be visited first.
//
// The type-parameter may reduce the number of instances visited in some cases, filtering out obvious non-matches.
func (tc *TypeClass) MatchInstance(param Type, found func(*Instance) bool) (matched bool) {
	seen := util.NewUintDedupeMap()
	switch param := param.(type) {
	case *Const:
		matched, _ = tc.matchConstInstance(param.Name, seen, found)
	case *App:
		if c, ok := param.Const.(*Const); ok {
			matched, _ = tc.matchAppConstInstance(c.Name, seen, found)
		} else {
			matched, _ = tc.matchInstance(seen, found)
		}
	case *Record:
		matched, _ = tc.matchRecordInstance(seen, found)
	case *Variant:
		matched, _ = tc.matchVariantInstance(seen, found)
	default:
		matched, _ = tc.matchInstance(seen, found)
	}
	seen.Release()
	return
}

// Visit all instances for each of the type-class's top-most parents and all their (transitive) sub-classes. Sub-classes will be visited first.
func (tc *TypeClass) FindInstanceFromRoots(found func(*Instance) bool) bool {
	roots := make(map[uint]*TypeClass, 16)
	tc.findRoots(roots)
	var (
		ok             bool
		shouldContinue bool
	)
	seen := util.NewUintDedupeMap()
	for _, root := range roots {
		if root == nil {
			// not a root
			continue
		}
		if ok, shouldContinue = root.findInstance(seen, found); !shouldContinue {
			break
		}
	}
	seen.Release()
	return ok
}

func (tc *TypeClass) findRoots(roots map[uint]*TypeClass) {
	if _, seen := roots[tc.Id]; seen {
		return
	}
	if len(tc.Super) == 0 {
		roots[tc.Id] = tc
		return
	}
	roots[tc.Id] = nil
	for _, super := range tc.Super {
		super.findRoots(roots)
	}
}

func (tc *TypeClass) findInstance(seen util.UintDedupeMap, found func(*Instance) bool) (ok, shouldContinue bool) {
	if seen[tc.Id] {
		return false, true
	}
	seen[tc.Id] = true
	for _, sub := range tc.Sub {
		if ok, shouldContinue = sub.findInstance(seen, found); !shouldContinue {
			return ok, false
		}
	}
	for _, inst := range tc.Instances {
		if found(inst) {
			return true, false
		}
	}
	return false, true
}

func (tc *TypeClass) matchInstance(seen util.UintDedupeMap, found func(*Instance) bool) (ok, shouldContinue bool) {
	if seen[tc.Id] {
		return false, true
	}
	seen[tc.Id] = true
	for _, sub := range tc.Sub {
		if ok, shouldContinue = sub.matchInstance(seen, found); !shouldContinue {
			return ok, false
		}
	}
	for _, inst := range tc.tmisc {
		if found(inst) {
			return true, false
		}
	}
	return false, true
}

func (tc *TypeClass) matchConstInstance(name string, seen util.UintDedupeMap, found func(*Instance) bool) (ok, shouldContinue bool) {
	if seen[tc.Id] {
		return false, true
	}
	seen[tc.Id] = true
	for _, sub := range tc.Sub {
		if ok, shouldContinue = sub.matchConstInstance(name, seen, found); !shouldContinue {
			return ok, false
		}
	}
	if tc.tconst != nil {
		inst := tc.tconst[name]
		if inst != nil && found(inst) {
			return true, false
		}
	}
	return false, true
}

func (tc *TypeClass) matchAppConstInstance(name string, seen util.UintDedupeMap, found func(*Instance) bool) (ok, shouldContinue bool) {
	if seen[tc.Id] {
		return false, true
	}
	seen[tc.Id] = true
	for _, sub := range tc.Sub {
		if ok, shouldContinue = sub.matchAppConstInstance(name, seen, found); !shouldContinue {
			return ok, false
		}
	}
	if tc.tappconst != nil {
		for _, inst := range tc.tappconst[name] {
			if found(inst) {
				return true, false
			}
		}
	}
	return false, true
}

func (tc *TypeClass) matchRecordInstance(seen util.UintDedupeMap, found func(*Instance) bool) (ok, shouldContinue bool) {
	if seen[tc.Id] {
		return false, true
	}
	seen[tc.Id] = true
	for _, sub := range tc.Sub {
		if ok, shouldContinue = sub.matchRecordInstance(seen, found); !shouldContinue {
			return ok, false
		}
	}
	for _, inst := range tc.trecord {
		if found(inst) {
			return true, false
		}
	}
	return false, true
}

func (tc *TypeClass) matchVariantInstance(seen util.UintDedupeMap, found func(*Instance) bool) (ok, shouldContinue bool) {
	if seen[tc.Id] {
		return false, true
	}
	seen[tc.Id] = true
	for _, sub := range tc.Sub {
		if ok, shouldContinue = sub.matchVariantInstance(seen, found); !shouldContinue {
			return ok, false
		}
	}
	for _, inst := range tc.tvariant {
		if found(inst) {
			return true, false
		}
	}
	return false, true
}

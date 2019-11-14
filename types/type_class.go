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
	// Id should be unique
	Id int
	// Name should be unique
	Name      string
	Param     Type
	Methods   MethodSet
	Super     map[int]*TypeClass
	Sub       map[int]*TypeClass
	Instances []*Instance
}

// Instance of a parameterized type-class
type Instance struct {
	TypeClass *TypeClass
	Param     Type
	Methods   MethodSet
	// MethodNames maps method names to names of their implementations within the type-environment.
	MethodNames map[string]string
}

// InstanceConstraint constrains a type-variable to types which implement a type-class.
type InstanceConstraint struct {
	TypeClass *TypeClass
}

// Create a new named/parameterized type-class with a set of method declarations.
func NewTypeClass(id int, name string, param Type, methods MethodSet) *TypeClass {
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
		sub.Super = make(map[int]*TypeClass)
	}
	if super.Sub == nil {
		super.Sub = make(map[int]*TypeClass)
	}
	sub.Super[super.Id] = super
	super.Sub[sub.Id] = sub
}

// Add an instance to the type-class with param as the type-parameter.
//
// methodNames must map from method names to names of their implementations within the type-environment.
func (tc *TypeClass) AddInstance(param Type, methods MethodSet, methodNames map[string]string) *Instance {
	inst := &Instance{TypeClass: tc, Param: param, Methods: methods, MethodNames: methodNames}
	tc.Instances = append(tc.Instances, inst)
	return inst
}

// Check if a type-class is declared as a sub-class of another type-class.
func (tc *TypeClass) HasSuperClass(super *TypeClass) bool {
	seen := util.NewIntDedupeMap()
	found := tc.hasSuperClass(seen, super.Id)
	seen.Release()
	return found
}

func (tc *TypeClass) hasSuperClass(seen util.IntDedupeMap, id int) bool {
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
	seen := util.NewIntDedupeMap()
	ok, _ := tc.findInstance(seen, found)
	seen.Release()
	return ok
}

// Visit all instances for each of the type-class's top-most parents and all their (transitive) sub-classes. Sub-classes will be visited first.
func (tc *TypeClass) FindInstanceFromRoots(found func(*Instance) bool) bool {
	roots := make(map[int]*TypeClass, 16)
	tc.findRoots(roots)
	var (
		ok             bool
		shouldContinue bool
	)
	seen := util.NewIntDedupeMap()
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

func (tc *TypeClass) findRoots(roots map[int]*TypeClass) {
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

func (tc *TypeClass) findInstance(seen util.IntDedupeMap, found func(*Instance) bool) (ok, shouldContinue bool) {
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

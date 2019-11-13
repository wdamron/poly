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
	Name      string
	Param     Type
	Methods   MethodSet
	Super     map[string]*TypeClass
	Sub       map[string]*TypeClass
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
func NewTypeClass(name string, param Type, methods MethodSet) *TypeClass {
	return &TypeClass{Name: name, Param: param, Methods: methods}
}

// Add a super-class to the type-class. This is an alias for `super.AddSubClass(sub)`.
func (sub *TypeClass) AddSuperClass(super *TypeClass) { super.AddSubClass(sub) }

// Add a sub-class to the type-class.
func (super *TypeClass) AddSubClass(sub *TypeClass) {
	for _, tc := range super.Sub {
		if tc.Name == sub.Name {
			return
		}
	}
	if sub.Super == nil {
		sub.Super = make(map[string]*TypeClass)
	}
	if super.Sub == nil {
		super.Sub = make(map[string]*TypeClass)
	}
	sub.Super[super.Name] = super
	super.Sub[sub.Name] = sub
}

// Add an instance to the type-class with param as the type-parameter.
//
// methodNames must map from method names to names of their implementations within the type-environment.
func (tc *TypeClass) AddInstance(param Type, methods MethodSet, methodNames map[string]string) *Instance {
	inst := &Instance{TypeClass: tc, Param: param, Methods: methods, MethodNames: methodNames}
	tc.Instances = append(tc.Instances, inst)
	return inst
}

// Check if a type-class has a super-class with a given name.
func (tc *TypeClass) HasSuperClass(name string) bool {
	seen := util.NewDedupeMap()
	found := tc.hasSuperClass(seen, name)
	seen.Release()
	return found
}

func (tc *TypeClass) hasSuperClass(seen map[string]bool, name string) bool {
	seen[tc.Name] = true
	for _, super := range tc.Super {
		switch {
		case seen[super.Name]:
			return false
		case super.Name == name, super.hasSuperClass(seen, name):
			return true
		}
	}
	return false
}

// Visit all instances for the type-class and all sub-classes. Sub-classes will be visited first.
func (tc *TypeClass) FindInstance(found func(*Instance) bool) bool {
	seen := util.NewDedupeMap()
	ok, _ := tc.findInstance(seen, found)
	seen.Release()
	return ok
}

// Visit all instances for each of the type-class's top-most parents and all their (transitive) sub-classes. Sub-classes will be visited first.
func (tc *TypeClass) FindInstanceFromRoots(found func(*Instance) bool) bool {
	roots := make(map[string]*TypeClass, 32)
	tc.findRoots(roots)
	var (
		ok             bool
		shouldContinue bool
	)
	seen := util.NewDedupeMap()
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

func (tc *TypeClass) findRoots(roots map[string]*TypeClass) {
	if _, seen := roots[tc.Name]; seen {
		return
	}
	if len(tc.Super) == 0 {
		roots[tc.Name] = tc
		return
	}
	roots[tc.Name] = nil
	for _, super := range tc.Super {
		super.findRoots(roots)
	}
}

func (tc *TypeClass) findInstance(seen map[string]bool, found func(*Instance) bool) (ok, shouldContinue bool) {
	if seen[tc.Name] {
		return false, true
	}
	seen[tc.Name] = true
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

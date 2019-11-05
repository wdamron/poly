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

package poly

import (
	"errors"

	"github.com/wdamron/poly/internal/util"
	"github.com/wdamron/poly/types"
)

// TypeEnv is a type-enviroment containing mappings from identifiers to declared types.
type TypeEnv struct {
	// Next unused type-variable id
	NextVarId int
	// Next unused qualified-type id
	NextQualifiedTypeId int
	// Next unused type-matcher id
	NextMatcherId int
	// Predeclared types in the parent of the current type-environment
	Parent *TypeEnv
	// Mappings from identifiers to declared types in the current type-environment
	Types map[string]types.Type
}

// Create a type-environment. The new environment will inherit bindings from the parent, if the parent is not nil.
func NewTypeEnv(parent *TypeEnv) *TypeEnv {
	env := &TypeEnv{
		Parent: parent,
		Types:  make(map[string]types.Type),
	}
	if parent != nil {
		env.NextVarId = parent.NextVarId
		env.NextQualifiedTypeId = parent.NextQualifiedTypeId
		env.NextMatcherId = parent.NextMatcherId
	}
	return env
}

func (e *TypeEnv) freshId() int {
	id := e.NextVarId
	e.NextVarId++
	return id
}

// Create a generic type-variable with a unique id.
func (e *TypeEnv) NewGenericVar() *types.Var { return types.NewGenericVar(e.freshId()) }

// Create a qualified type-variable with a unique id.
func (e *TypeEnv) NewQualifiedVar(constraints ...types.InstanceConstraint) *types.Var {
	tv := types.NewGenericVar(e.freshId())
	tv.SetConstraints(constraints)
	return tv
}

// Declare a type for an identifier within the type environment.
func (e *TypeEnv) Add(name string, t types.Type) { e.Types[name] = generalize(-1, t) }

// Remove a declared type for an identifier from the type environment.
func (e *TypeEnv) Remove(name string) { delete(e.Types, name) }

// Lookup the type for an identifier in the environment or its parent environment(s).
func (e *TypeEnv) Lookup(name string) (types.Type, bool) {
	if t, ok := e.Types[name]; ok {
		return t, ok
	}
	if e.Parent == nil {
		return nil, false
	}
	return e.Parent.Lookup(name)
}

// Declare a parameterized type-class within the type environment.
func (e *TypeEnv) NewTypeClass(name string, param types.Type, methods map[string]*types.Arrow) *types.TypeClass {
	methods2 := make(map[string]*types.Arrow, len(methods))
	tc := types.NewTypeClass(name, generalize(-1, param), methods2)
	for name, arrow := range methods {
		arrow = generalize(-1, arrow).(*types.Arrow)
		methods2[name] = arrow
		e.Types[name] = &types.Method{TypeClass: tc, Name: name, HasGenericVars: arrow.HasGenericVars}
	}
	return tc
}

// Declare an instance for a parameterized type-class within the type environment. The instance must implement
// all methods for the type-class and all parents of the type-class. The instance must not overlap with (i.e. unify with)
// any other instances for the type-class, though this will not be checked.
func (e *TypeEnv) NewInstance(tc *types.TypeClass, param types.Type, methodImpls map[string]string) (*types.Instance, error) {
	seen := util.NewDedupeMap()
	err := e.checkSatisfies(tc, methodImpls, seen)
	seen.Release()
	if err != nil {
		return nil, err
	}
	impls := make(map[string]*types.Arrow, len(methodImpls))
	for name, implName := range methodImpls {
		impl, ok := e.Lookup(implName)
		if !ok {
			return nil, errors.New("Missing method implementation " + methodImpls[name] + " for " + name)
		}
		arrow, ok := impl.(*types.Arrow)
		if !ok {
			return nil, errors.New("Method implementation " + methodImpls[name] + " for " + name + "is not a function")
		}
		impls[name] = arrow
	}
	return tc.AddInstance(generalize(-1, param), impls), nil
}

func (e *TypeEnv) checkSatisfies(tc *types.TypeClass, methodImpls map[string]string, seen map[string]bool) error {
	for name := range tc.Methods {
		if _, ok := methodImpls[name]; !ok {
			return errors.New("Missing method implementation for " + name + " of type-class " + tc.Name)
		}
	}
	seen[tc.Name] = true
	for _, super := range tc.Super {
		if seen[super.Name] {
			continue
		}
		if err := e.checkSatisfies(super, methodImpls, seen); err != nil {
			return err
		}
	}
	return nil
}

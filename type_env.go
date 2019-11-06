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
//
// A type-environment cannot be used concurrently for inference; to share a type-environment
// across threads, create a new type-environment for each thread which inherits from the
// shared environment.
type TypeEnv struct {
	// Next unused type-variable id
	NextVarId int
	// Predeclared types in the parent of the current type-environment
	Parent *TypeEnv
	// Mappings from identifiers to declared types in the current type-environment
	Types map[string]types.Type

	common *commonContext
}

// Create a type-environment. The new environment will inherit bindings from the parent, if the parent is not nil.
//
// A type-environment cannot be used concurrently for inference; to share a type-environment
// across threads, create a new type-environment for each thread which inherits from the
// shared environment.
func NewTypeEnv(parent *TypeEnv) *TypeEnv {
	env := &TypeEnv{
		Parent: parent,
		Types:  make(map[string]types.Type),
	}
	if parent != nil {
		env.NextVarId = parent.NextVarId
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
func (e *TypeEnv) Declare(name string, t types.Type) { e.Types[name] = generalize(-1, t) }

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
func (e *TypeEnv) DeclareTypeClass(name string, param types.Type, methods map[string]*types.Arrow) (*types.TypeClass, error) {
	if _, isFunction := param.(*types.Arrow); isFunction {
		return nil, errors.New("Unsupported function parameter for type-class " + name)
	}
	methods2 := make(map[string]*types.Arrow, len(methods))
	tc := types.NewTypeClass(name, generalize(-1, param), methods2)
	for name, arrow := range methods {
		arrow = generalize(-1, arrow).(*types.Arrow)
		methods2[name] = arrow
		e.Types[name] = &types.Method{TypeClass: tc, Name: name, HasGenericVars: arrow.HasGenericVars}
	}
	return tc, nil
}

// Declare an instance for a parameterized type-class within the type environment. The instance must implement
// all methods for the type-class and all parents of the type-class. The instance must not overlap with (i.e. unify with)
// any other instances for the type-class, though this will not be checked.
//
// methodImpls must map from method names to names of their implementations.
func (e *TypeEnv) DeclareInstance(tc *types.TypeClass, param types.Type, methodImpls map[string]string) (*types.Instance, error) {
	if _, isFunction := param.(*types.Arrow); isFunction {
		return nil, errors.New("Unsupported function instance for type-class " + tc.Name)
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
	seen := util.NewDedupeMap()
	if e.common == nil {
		e.common = &commonContext{}
		e.common.init()
	}
	inst := tc.AddInstance(generalize(-1, param), impls)
	err := e.checkSatisfies(tc, param, impls, seen)
	seen.Release()
	if err != nil {
		tc.Instances = tc.Instances[:len(tc.Instances)-1]
		return nil, err
	}
	return inst, nil
}

func (e *TypeEnv) checkSatisfies(tc *types.TypeClass, param types.Type, methodImpls map[string]*types.Arrow, seen map[string]bool) error {
	for name, def := range tc.Methods {
		impl, ok := methodImpls[name]
		if !ok || len(def.Args) != len(impl.Args) {
			return methodErr(tc, param, name, nil)
		}
		speculating := e.common.speculate
		e.common.speculate = true
		stashedLinks := e.common.linkStash
		var err error
		for i, arg := range def.Args {
			if err = e.common.unify(e.common.instantiate(0, arg), e.common.instantiate(0, impl.Args[i])); err != nil {
				break
			}
		}
		e.common.unstashLinks(len(e.common.linkStash) - len(stashedLinks))
		e.common.speculate, e.common.linkStash = speculating, stashedLinks
		if err != nil {
			return methodErr(tc, param, name, err)
		}
	}
	seen[tc.Name] = true
	for _, super := range tc.Super {
		if seen[super.Name] {
			continue
		}
		if err := e.checkSatisfies(super, param, methodImpls, seen); err != nil {
			return err
		}
	}
	return nil
}

// TODO: use wrapped errors
func methodErr(tc *types.TypeClass, param types.Type, method string, err error) error {
	str := "type " + types.TypeString(param) + " does not implement method " + method + " of type-class " + tc.Name
	if err != nil {
		str += " -- " + err.Error()
	}
	return errors.New(str)
}

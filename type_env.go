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

// Create a unbound type-variable with a unique id at a given binding-level.
func (e *TypeEnv) NewVar(level int) *types.Var { return types.NewVar(e.freshId(), level) }

// Create a generic type-variable with a unique id.
func (e *TypeEnv) NewGenericVar() *types.Var { return types.NewGenericVar(e.freshId()) }

// Create a qualified type-variable with a unique id.
func (e *TypeEnv) NewQualifiedVar(constraints ...types.InstanceConstraint) *types.Var {
	tv := types.NewGenericVar(e.freshId())
	for _, c := range constraints {
		tv.AddConstraint(c)
	}
	return tv
}

// Declare a type for an identifier within the type environment.
func (e *TypeEnv) Declare(name string, t types.Type) { e.Types[name] = forceGeneralize(-1, t) }

// Declare a weakly-generalized type for an identifier within the type environment.
// Type-variables contained within mutable reference-types will not be generalized.
func (e *TypeEnv) DeclareWeak(name string, t types.Type) { e.Types[name] = generalize(-1, t) }

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
//
// If the type-parameter is not linked within the bind function, an instance constraint will be added to the parameter.
func (e *TypeEnv) DeclareTypeClass(name string, bind func(*types.Var) types.MethodSet, implements ...*types.TypeClass) (*types.TypeClass, error) {
	param := e.NewGenericVar()
	methods := bind(param)
	if param.IsLinkVar() {
		if _, isFunction := types.RealType(param.Link()).(*types.Arrow); isFunction {
			return nil, errors.New("Unsupported function parameter for type-class " + name)
		}
	}
	generalizedMethods := make(types.MethodSet, len(methods))
	tc := types.NewTypeClass(name, forceGeneralize(-1, param), generalizedMethods)
	for name, arrow := range methods {
		arrow = forceGeneralize(-1, arrow).(*types.Arrow)
		generalizedMethods[name] = arrow
		e.Types[name] = &types.Method{TypeClass: tc, Name: name, Flags: arrow.Flags}
	}
	if param.IsGeneric() {
		param.AddConstraint(types.InstanceConstraint{tc})
	} else {
		tc.Param = types.RealType(tc.Param)
	}
	for _, super := range implements {
		super.AddSubClass(tc)
	}
	return tc, nil
}

// Declare an instance for a parameterized type-class within the type environment. The instance must implement
// all methods for the type-class and all parents of the type-class. The instance type must not overlap with (i.e. unify with)
// any other instances for the type-class.
//
// methodNames must map from method names to names of their implementations within the type-environment.
func (e *TypeEnv) DeclareInstance(tc *types.TypeClass, param types.Type, methodNames map[string]string) (*types.Instance, error) {
	if _, isFunction := param.(*types.Arrow); isFunction {
		return nil, errors.New("Unsupported function instance for type-class " + tc.Name)
	}
	// prevent overlapping instances:
	var conflict *types.Instance
	tc.FindInstanceFromRoots(func(inst *types.Instance) bool {
		if !e.common.canUnify(e.common.instantiate(0, param), e.common.instantiate(0, inst.Param)) {
			return false
		}
		if inst.TypeClass.HasSuperClass(tc.Name) || tc.HasSuperClass(inst.TypeClass.Name) {
			return false
		}
		conflict = inst
		return true
	})
	if conflict != nil {
		return nil, errors.New("Found overlapping instance for type-class " + tc.Name + " at " + conflict.TypeClass.Name + " instance " + types.TypeString(conflict.Param))
	}

	impls := make(types.MethodSet, len(methodNames))
	for name, implName := range methodNames {
		impl, ok := e.Lookup(implName)
		if !ok {
			return nil, errors.New("Missing method implementation " + methodNames[name] + " for " + name)
		}
		arrow, ok := impl.(*types.Arrow)
		if !ok {
			return nil, errors.New("Method implementation " + methodNames[name] + " for " + name + "is not a function")
		}
		impls[name] = arrow
	}
	if e.common == nil {
		e.common = &commonContext{}
		e.common.init()
	}
	param = forceGeneralize(-1, param)
	inst := tc.AddInstance(param, impls, methodNames)
	seen := util.NewDedupeMap()
	err := e.checkSatisfies(tc, param, impls, seen)
	seen.Release()
	e.common.varTracker.FlattenLinks()
	e.common.varTracker.ResetKeepId()
	if err != nil {
		tc.Instances = tc.Instances[:len(tc.Instances)-1]
		return nil, err
	}
	return inst, nil
}

// Find the type-class instance which implements a called function's underlying method.
//
// arrow should be the function-type assigned to a Call expression during inference.
// If the Call expression was not inferred to be a method call, a nil instance will be returned.
func (e *TypeEnv) FindMethodInstance(arrow *types.Arrow) *types.Instance {
	method := arrow.Method
	if method == nil {
		return nil
	}
	if e.common == nil {
		e.common = &commonContext{}
		e.common.init()
	}
	var match *types.Instance
	method.TypeClass.FindInstance(func(inst *types.Instance) bool {
		if !e.common.canUnify(e.common.instantiate(0, arrow), e.common.instantiate(0, inst.Methods[method.Name])) {
			return false
		}
		match = inst
		return true
	})
	return match
}

func (e *TypeEnv) checkSatisfies(tc *types.TypeClass, param types.Type, methodImpls types.MethodSet, seen map[string]bool) error {
	for name, def := range tc.Methods {
		impl, ok := methodImpls[name]
		if !ok || len(def.Args) != len(impl.Args) {
			return methodErr(tc, param, name)
		}
		if !e.common.canUnify(e.common.instantiate(0, def).(*types.Arrow), e.common.instantiate(0, impl).(*types.Arrow)) {
			return methodErr(tc, param, name)
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
func methodErr(tc *types.TypeClass, param types.Type, method string) error {
	return errors.New("Type " + types.TypeString(param) + " does not implement method " + method + " of type-class " + tc.Name)
}

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

	"github.com/wdamron/poly/internal/typeutil"
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
	// Type-classes declared in the current type-environment
	TypeClasses map[string]*types.TypeClass

	common *typeutil.CommonContext
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

// Create a new recursive type or group of mutually-recursive types.
//
// The bind function should add aliased types with underlying types which are recursively linked
// to one or more types in the Recursive.
func (e *TypeEnv) NewRecursive(bind func(*types.Recursive)) (*types.Recursive, error) {
	rec := &types.Recursive{Id: e.freshId()}
	bind(rec)
	for i, ti := range rec.Types {
		// Generalizing any of the recursive types will generalize the Recursive (once)
		rec.Types[i] = types.GeneralizeWeak(ti).(*types.App)
	}
	return rec, nil
}

// Create a new recursive type.
//
// The bindSelf function should add a single aliased type with an underlying type which is recursively linked
// to the Recursive.
func (e *TypeEnv) NewSimpleRecursive(bindSelf func(*types.Recursive, *types.RecursiveLink)) (*types.Recursive, error) {
	bind := func(rec *types.Recursive) {
		bindSelf(rec, &types.RecursiveLink{Recursive: rec, Index: 0})
	}
	rec, err := e.NewRecursive(bind)
	if err != nil {
		return nil, err
	}
	if len(rec.Types) != 1 {
		return nil, errors.New("Simple recursive type groups must contain a single recursive type")
	}
	return rec, nil
}

// Create a new recursive type or group of mutually-recursive types which instantiates (unifies with) a generic
// recursive type of group of mutually-recursive types.
//
// The bind function should add aliased types with underlying types which are recursively linked to one or more
// types in the Recursive.
func (e *TypeEnv) NewRecursiveInstance(parent *types.Recursive, bind func(*types.Recursive)) (*types.Recursive, error) {
	rec := &types.Recursive{Id: parent.Id}
	bind(rec)
	if len(rec.Types) != len(parent.Types) {
		return nil, errors.New("Recursive instance must have the same number of types as the parent group")
	}
	for i, ti := range rec.Types {
		// Generalizing any of the recursive types will generalize the Recursive (once)
		rec.Types[i] = types.GeneralizeWeak(ti).(*types.App)
	}
	if e.common == nil {
		e.common = &typeutil.CommonContext{}
		e.common.Init()
	}
	for i, ti := range rec.Types {
		if !e.common.CanUnify(e.common.Instantiate(types.TopLevel, ti), e.common.Instantiate(types.TopLevel, parent.Types[i])) {
			return nil, errors.New("Recursive instance must instantiate (unify with) the parent group")
		}
	}
	return rec, nil
}

// Create a new recursive type which instantiates (unifies with) a generic recursive type.
//
// The bindSelf function should add a single aliased type with an underlying type which is recursively linked
// to the Recursive.
func (e *TypeEnv) NewSimpleRecursiveInstance(parent *types.Recursive, bindSelf func(*types.Recursive, *types.RecursiveLink)) (*types.Recursive, error) {
	bind := func(rec *types.Recursive) {
		bindSelf(rec, &types.RecursiveLink{Recursive: rec, Index: 0})
	}
	rec, err := e.NewRecursiveInstance(parent, bind)
	if err != nil {
		return nil, err
	}
	if len(rec.Types) != 1 {
		return nil, errors.New("Simple recursive type groups must contain a single recursive type")
	}
	return rec, nil
}

// Declare a type for an identifier within the type environment.
//
// Type-variables contained within mutable reference-types will be generalized.
func (e *TypeEnv) Declare(name string, t types.Type) {
	e.Types[name] = types.GeneralizeRefs(t)
}

// Declare a weakly-polymorphic type for an identifier within the type environment.
//
// Type-variables contained within mutable reference-types will not be generalized.
func (e *TypeEnv) DeclareWeak(name string, t types.Type) {
	e.Types[name] = types.Generalize(t)
}

// Declare a type for an identifier within the type environment.
//
// Type-variables will not be generalized.
func (e *TypeEnv) DeclareInvariant(name string, t types.Type) { e.Types[name] = t }

// Declare a type for an identifier within the type environment.
//
// Type-variables will not be generalized.
//
// Assign is an alias for DeclareInvariant.
func (e *TypeEnv) Assign(name string, t types.Type) { e.Types[name] = t }

// Remove the assigned type for an identifier within the type environment. Parent environment(s) will not be affected,
// and the identifier's type will still be visible if defined in a parent environment.
func (e *TypeEnv) Remove(name string) { delete(e.Types, name) }

// Lookup the type for an identifier in the environment or its parent environment(s).
func (e *TypeEnv) Lookup(name string) types.Type {
	if t, ok := e.Types[name]; ok {
		return t
	}
	if e.Parent == nil {
		return nil
	}
	return e.Parent.Lookup(name)
}

// Declare a parameterized type-class within the type environment.
//
// If the type-parameter is not linked within the bind function, an instance constraint will be added to the parameter.
//
// Each super-class which the type-class implements will be modified to add a sub-class entry; changes will be visible across all uses
// of the super-classes, and changes must not be made to type-classes concurrently.
func (e *TypeEnv) DeclareTypeClass(name string, bind func(*types.Var) types.MethodSet, implements ...*types.TypeClass) (*types.TypeClass, error) {
	if existing := e.LookupTypeClass(name); existing != nil {
		return nil, errors.New("Type-class " + name + " is already declared")
	}
	param := e.NewGenericVar()
	methods := bind(param)
	if param.IsLinkVar() {
		if _, isFunction := types.RealType(param.Link()).(*types.Arrow); isFunction {
			return nil, errors.New("Unsupported function parameter for type-class " + name)
		}
	}
	generalizedMethods := make(types.MethodSet, len(methods))
	tc := types.NewTypeClass(e.freshId(), name, types.GeneralizeRefs(param), generalizedMethods)
	for name, arrow := range methods {
		arrow = types.GeneralizeRefs(arrow).(*types.Arrow)
		generalizedMethods[name] = arrow
		e.Types[name] = &types.Method{TypeClass: tc, Name: name, Flags: arrow.Flags}
	}
	if param.IsGeneric() {
		param.AddConstraint(types.InstanceConstraint{tc})
	} else {
		tc.Param = types.RealType(tc.Param)
	}
	if e.TypeClasses == nil {
		e.TypeClasses = make(map[string]*types.TypeClass)
	}
	e.TypeClasses[name] = tc
	for _, super := range implements {
		super.AddSubClass(tc)
	}
	return tc, nil
}

// Declare a closed union (a.k.a. sum/variant/enum) type-class within the type environment. This is a shortcut for declaring a type-class with an
// empty method-set and no super-classes then declaring the given instances for the type-class.
//
// If the bind function is nil or the type-parameter is not linked within the bind function, an instance constraint will be added to the parameter.
//
// A union type-class represents a named/closed set of types, whereas tagged (ad-hoc) variant-types are anonymous/open sets of types.
// Functions may be parameterized over a union type-class; only tagged (ad-hoc) variant-types may be used in (tag-based) match expressions.
// Match expressions may offer less flexibility compared to (unification-driven) function overloading with instance constraints.
//
// Each super-class which the type-class implements will be modified to add a sub-class entry; changes will be visible across all uses
// of the super-classes, and changes must not be made to type-classes concurrently.
func (e *TypeEnv) DeclareUnionTypeClass(name string, bind func(*types.Var), instances ...types.Type) (*types.TypeClass, error) {
	bindMethods := func(param *types.Var) types.MethodSet {
		if bind != nil {
			bind(param)
		}
		return types.MethodSet{}
	}
	tc, err := e.DeclareTypeClass(name, bindMethods)
	if err != nil {
		return nil, err
	}
	for _, inst := range instances {
		if _, err := e.DeclareInstance(tc, inst, map[string]string{}); err != nil {
			return nil, err
		}
	}
	return tc, nil
}

// Lookup a declared type-class in the environment or its parent environment(s).
func (e *TypeEnv) LookupTypeClass(name string) *types.TypeClass {
	if e.TypeClasses != nil {
		if tc, ok := e.TypeClasses[name]; ok {
			return tc
		}
	}
	if e.Parent == nil {
		return nil
	}
	return e.Parent.LookupTypeClass(name)
}

// Declare an instance for a parameterized type-class within the type environment. The instance must implement
// all methods for the type-class and all parents of the type-class. The instance type must not overlap with (i.e. unify with)
// any other instances for the type-class.
//
// methodNames must map from method names to names of their implementations within the type-environment.
//
// The type-class which the instance implements will be modified to add an instance entry; changes will be visible across all uses
// of the type-class, and changes must not be made to type-classes concurrently.
func (e *TypeEnv) DeclareInstance(tc *types.TypeClass, param types.Type, methodNames map[string]string) (*types.Instance, error) {
	if _, isFunction := param.(*types.Arrow); isFunction {
		return nil, errors.New("Unsupported function instance for type-class " + tc.Name)
	}
	// prevent overlapping instances:
	var conflict *types.Instance
	tc.FindInstanceFromRoots(func(inst *types.Instance) bool {
		if !e.common.CanUnify(e.common.Instantiate(0, param), e.common.Instantiate(0, inst.Param)) {
			return false
		}
		if inst.TypeClass.HasSuperClass(tc) || tc.HasSuperClass(inst.TypeClass) {
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
		impl := e.Lookup(implName)
		if impl == nil {
			return nil, errors.New("Missing method implementation " + methodNames[name] + " for " + name)
		}
		arrow, ok := impl.(*types.Arrow)
		if !ok {
			return nil, errors.New("Method implementation " + methodNames[name] + " for " + name + "is not a function")
		}
		impls[name] = arrow
	}
	if e.common == nil {
		e.common = &typeutil.CommonContext{}
		e.common.Init()
	}
	param = types.GeneralizeRefs(param)
	inst := tc.AddInstance(param, impls, methodNames)
	seen := util.NewIntDedupeMap()
	err := e.checkSatisfies(tc, param, impls, seen)
	seen.Release()
	e.common.VarTracker.FlattenLinks()
	e.common.VarTracker.ResetKeepId()
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
		e.common = &typeutil.CommonContext{}
		e.common.Init()
	}
	var match *types.Instance
	method.TypeClass.FindInstance(func(inst *types.Instance) bool {
		if !e.common.CanUnify(e.common.Instantiate(0, arrow), e.common.Instantiate(0, inst.Methods[method.Name])) {
			return false
		}
		match = inst
		return true
	})
	return match
}

func (e *TypeEnv) checkSatisfies(tc *types.TypeClass, param types.Type, methodImpls types.MethodSet, seen util.IntDedupeMap) error {
	for name, def := range tc.Methods {
		impl, ok := methodImpls[name]
		if !ok || len(def.Args) != len(impl.Args) {
			return methodErr(tc, param, name)
		}
		if !e.common.CanUnify(e.common.Instantiate(0, def).(*types.Arrow), e.common.Instantiate(0, impl).(*types.Arrow)) {
			return methodErr(tc, param, name)
		}
	}
	seen[tc.Id] = true
	for superId, super := range tc.Super {
		if seen[superId] {
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

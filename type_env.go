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

	"github.com/wdamron/poly/ast"
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
	// Mappings from identifiers to declared types in the current type-environment
	Types map[string]types.Type
	// Type-classes declared in the current type-environment
	TypeClasses map[string]*types.TypeClass
	// Predeclared types in the parent of the current type-environment
	Parent *TypeEnv

	common typeutil.CommonContext
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
	env.common.Init()
	if parent != nil {
		env.common.VarTracker.NextId = parent.common.VarTracker.NextId
	}
	return env
}

// Get the id which will be assigned to the next type-variable generated within the type-environment.
func (e *TypeEnv) NextVarId() uint { return e.common.VarTracker.NextId }

func (e *TypeEnv) freshId() uint {
	id := e.common.VarTracker.NextId
	e.common.VarTracker.NextId++
	return id
}

// Create a unbound type-variable with a unique id at a given binding-level.
func (e *TypeEnv) NewVar(level uint) *types.Var { return types.NewVar(e.freshId(), level) }

// Create a generic type-variable with a unique id.
func (e *TypeEnv) NewGenericVar() *types.Var { return types.NewGenericVar(e.freshId()) }

// Create a generic size type-variable with a unique id. Size type-variables must link to size types.
func (e *TypeEnv) NewGenericSize() *types.Var {
	tv := e.NewGenericVar()
	tv.RestrictSizeVar()
	return tv
}

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
func (e *TypeEnv) NewRecursive(params []*types.Var, bind func(recursive *types.Recursive)) *types.Recursive {
	rec := &types.Recursive{Params: params, Bind: bind, Flags: types.NeedsGeneralization}
	for i, tv := range rec.Params {
		rec.Params[i] = Generalize(tv).(*types.Var)
	}
	bind(rec)
	for i, alias := range rec.Types {
		rec.Types[i] = Generalize(alias).(*types.App)
	}
	return rec
}

// Create a new recursive type.
//
// The bindSelf function should add a single aliased type with an underlying type which is recursively linked
// to the Recursive.
func (e *TypeEnv) NewSimpleRecursive(params []*types.Var, bindSelf func(*types.Recursive, *types.RecursiveLink)) *types.Recursive {
	return e.NewRecursive(params, func(rec *types.Recursive) {
		bindSelf(rec, &types.RecursiveLink{Recursive: rec, Index: 0})
	})
}

// Declare a type for an identifier within the type environment.
//
// Type-variables contained within mutable reference-types will be generalized.
func (e *TypeEnv) Declare(name string, t types.Type) {
	e.Types[name] = GeneralizeRefs(t)
}

// Declare a weakly-polymorphic type for an identifier within the type environment.
//
// Type-variables contained within mutable reference-types will not be generalized.
func (e *TypeEnv) DeclareWeak(name string, t types.Type) {
	e.Types[name] = Generalize(t)
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

func (e *TypeEnv) scopeLookup(name string) (types.Type, *ast.Scope) {
	if t, ok := e.Types[name]; ok {
		scopes := e.common.VarScopes[name]
		if len(scopes) == 0 {
			return t, ast.PredeclaredScope
		}
		return t, scopes[len(scopes)-1]
	}
	if e.Parent == nil {
		return nil, nil
	}
	return e.Parent.scopeLookup(name)
}

// Instantiate a type at a given let-binding level. Instantiation should only occur indirectly during inference.
//
// Literal expressions may need to instantiate types at the level they are being instantiated at.
func (e *TypeEnv) Instantiate(level uint, t types.Type) types.Type {
	return e.common.Instantiate(level, t)
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
	tc := types.NewTypeClass(e.freshId(), name, GeneralizeRefs(param), generalizedMethods)
	for name, arrow := range methods {
		arrow = GeneralizeRefs(arrow).(*types.Arrow)
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

// Declare a closed union (a.k.a. sum/variant/enum) type-class within the type environment. This is a shortcut for declaring a type-class
// with an empty method-set and no super-classes then declaring the given instances for the type-class.
//
// Additionally, a helper function will be declared in the type environment with the same name as the type-class, which converts an
// instance of the type-class to a tagged (ad-hoc) variant-type which may be used in (tag-based) match expressions. The variant-type
// will be labeled with the name and type of each instance.
//
// If the bind function is nil or the type-parameter is not linked within the bind function, an instance constraint will be added to
// the parameter.
//
// A union type-class represents a named/closed set of types, whereas tagged (ad-hoc) variant-types are anonymous/open sets of types.
// Functions may be parameterized over a union type-class; only tagged (ad-hoc) variant-types may be used in (tag-based) match expressions.
// Match expressions may offer less flexibility compared to (unification-driven) function overloading with instance constraints.
//
// Each super-class which the type-class implements will be modified to add a sub-class entry; changes will be visible across all uses
// of the super-classes, and changes must not be made to type-classes concurrently.
func (e *TypeEnv) DeclareUnionTypeClass(name string, bind func(*types.Var), instances map[string]types.Type) (*types.TypeClass, error) {
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
	tc.Union = make(map[string]*types.Instance, len(instances))
	for label, param := range instances {
		inst, err := e.DeclareInstance(tc, param, map[string]string{})
		if err != nil {
			return nil, err
		}
		tc.Union[label] = inst
	}
	tc.UnionVariant = &types.Variant{
		Row: &types.RowExtend{Labels: types.NewFlatTypeMap(instances), Row: types.RowEmptyPointer},
	}
	e.Declare(name, &types.Arrow{
		Args:   []types.Type{e.NewQualifiedVar(types.InstanceConstraint{tc})},
		Return: tc.UnionVariant,
	})
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
	switch param.(type) {
	case *types.Const, *types.App, *types.Record, *types.Variant:
		// ok
	default:
		return nil, errors.New("Type-class instance must be a type constant, type application, record type, or variant type")
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
	param = GeneralizeRefs(param)
	inst := tc.AddInstance(param, impls, methodNames)
	seen := util.NewUintDedupeMap()
	err := e.checkSatisfies(tc, param, impls, seen)
	seen.Release()
	e.common.VarTracker.FlattenLinks()
	e.common.VarTracker.Reset()
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

func (e *TypeEnv) checkSatisfies(tc *types.TypeClass, param types.Type, methodImpls types.MethodSet, seen util.UintDedupeMap) error {
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

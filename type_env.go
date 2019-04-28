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
	"github.com/wdamron/poly/types"
)

// TypeEnv is a type-enviroment containing mappings from identifiers to declared types.
type TypeEnv struct {
	// Next unused type-variable id
	NextVarId int
	// Next unused kind id
	NextKindId int
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
		env.NextKindId = parent.NextKindId
	}
	return env
}

func (e *TypeEnv) freshId() int {
	id := e.NextVarId
	e.NextVarId++
	return id
}

// Create a new type-qualifier with the given name, which restricts types
// to the given set of type-constants.
func (e *TypeEnv) NewUnion(name string, anyOf ...*types.Const) *types.Kind {
	k := types.NewUnion(name, e.NextKindId, anyOf...)
	e.NextKindId++
	return k
}

// Create a generic type-variable with a unique id.
func (e *TypeEnv) NewGenericVar() *types.Var { return types.NewGenericVar(e.freshId()) }

// Create a qualified type-variable with a unique id.
func (e *TypeEnv) NewQualifiedVar(kinds ...*types.Kind) *types.Var {
	tv := types.NewGenericVar(e.freshId())
	tv.AddKinds(kinds)
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

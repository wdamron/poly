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

// TypeEnv contains mappings from identifiers to declared types.
type TypeEnv struct {
	env    map[string]types.Type // declared types
	nextId int                   // next unused type-variable id
}

// Create a type environment.
func NewTypeEnv() *TypeEnv { return &TypeEnv{env: make(map[string]types.Type)} }

// Create a fresh generic type-variable.
func (e *TypeEnv) NewGenericVar() *types.Var { return types.NewGenericVar(e.freshId()) }

// Declare a type for an identifier within the type environment.
func (e *TypeEnv) Add(name string, t types.Type) { e.env[name] = generalize(-1, t) }

// Remove a declared type for an identifier from the type environment.
func (e *TypeEnv) Remove(name string) { delete(e.env, name) }

// Get the map of declared types for the environment.
func (e *TypeEnv) Map() map[string]types.Type { return e.env }

func (e *TypeEnv) freshId() int {
	id := e.nextId
	e.nextId++
	return id
}

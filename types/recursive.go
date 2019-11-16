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

var _ Type = (*RecursiveLink)(nil)

// https://www.cs.cmu.edu/~rwh/papers/datatypes/tr.pdf

// Recursive is a recursive type or a group of mutually-recursive types.
type Recursive struct {
	// Id should uniquely identify the recursive type(s).
	Id int
	// Types is an indexed slice containing one or more aliased types.
	// Each aliased type should contain recursive links which point back to this recursive group.
	Types        []*App
	Flags        TypeFlags
	Generalized  bool
	Instantiated bool
}

// Add a type to the recursive type-group. The index of the type will be returned.
func (r *Recursive) AddType(aliased *App) int {
	r.Types = append(r.Types, aliased)
	return len(r.Types) - 1
}

// Recursive link to a type.
type RecursiveLink struct {
	Recursive *Recursive
	Index     int
}

// "Recursive"
func (t *RecursiveLink) TypeName() string { return "RecursiveLink" }

// Check if t contains generic types.
func (t *RecursiveLink) IsGeneric() bool { return t.Recursive.Flags&ContainsGenericVars != 0 }

// Check if t contains mutable reference-types.
func (t *RecursiveLink) HasRefs() bool { return t.Recursive.Flags&ContainsRefs != 0 }

// Expand the recursively-linked type for t.
func (t *RecursiveLink) Link() Type { return t.Recursive.Types[t.Index] }

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
	Names        []string
	Indexes      map[string]int
	Flags        TypeFlags
	Generalized  bool
	Instantiated bool
}

// Add a type to the recursive type-group. The index of the type will be returned.
//
// Each type in the recursive type-group must be assigned a unique name, which may be used for looking
// up the type's index within the group. The name is not printed or included with the type.
func (r *Recursive) AddType(name string, aliased *App) int {
	r.Types, r.Names = append(r.Types, aliased), append(r.Names, name)
	if r.Indexes == nil {
		r.Indexes = make(map[string]int)
	}
	r.Indexes[name] = len(r.Types) - 1
	return len(r.Types) - 1
}

// Lookup a type within the recursive type-group by its unique name.
func (r *Recursive) GetType(name string) *App {
	if len(r.Types) == 0 {
		return nil
	}
	if index, ok := r.Indexes[name]; ok {
		return r.Types[index]
	}
	return nil
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

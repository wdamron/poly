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

// Recursive is a recursive type or a group of mutually-recursive types.
//
// See https://www.cs.cmu.edu/~rwh/papers/datatypes/tr.pdf
//
// Recursive types are separated into their own subcategory, ranged over by δ.
// A recursive type δ has the form μi(α1,...,αn).(τ1,...,τn), where 1 ≤ i ≤ n and each αj
// is a type variable that may appear free in any or all of τ1,...,τn.
//
//   δ ::= μi(α1,...,αn).(τ1,...,τn)
//
// Intuitively, this type is the ith in a system of n mutually recursive types. As such,
// it is isomorphic to τi with each αj replaced by the jth component of the recursive bundle.
//
// Formally, it is isomorphic to the following type:
//
//   τi[μ1(α1,...,αn).(τ1,...,τn), ..., μn(α1,...,αn).(τ1,...,τn) / τ1,...,τn]
//
// where we denote by τ[σ1,...,σn/α1,...,αn] the simultaneous capture-avoiding substitution
// of σ1,...,σn for α1,...,αn in τ.
type Recursive struct {
	// Source is a recursive type which this type instantiates, or nil.
	Source *Recursive
	// Params are type-variables used by the recursive type(s).
	Params []*Var
	// Types is an indexed slice containing one or more aliased types.
	// Each aliased type should contain recursive links which point back to this recursive group.
	Types   []*App
	Names   []string
	Indexes map[string]int
	// Bind types to an instance of this recursive. The new instance may have
	// different type-parameters set before binding, if any of the existing
	// parameters require instantiation.
	Bind  func(instance *Recursive)
	Flags TypeFlags
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

// Check if r is declared as an instance of source.
func (r *Recursive) IsInstanceOf(source *Recursive) bool {
	if r == source {
		return true
	}
	for source := r.Source; source != nil; source = source.Source {
		if r == source {
			return true
		}
	}
	return false
}

// Check if r and other are instantiated from the same root.
func (r *Recursive) Matches(other *Recursive) bool {
	var rootA, rootB *Recursive
	for rootA = r; rootA != nil; rootA = rootA.Source {
	}
	for rootB = other; rootB != nil; rootB = rootB.Source {
	}
	return rootA == rootB
}

// Check if any type parameters for r contain generic types.
func (r *Recursive) IsGeneric() bool { return r.Flags&ContainsGenericVars != 0 }

// Check if any type parameters for r contain mutable reference-types.
func (r *Recursive) HasRefs() bool { return r.Flags&ContainsRefs != 0 }

// Check if r needs to be generalized.
func (r *Recursive) NeedsGeneralization() bool { return r.Flags&NeedsGeneralization != 0 }

// Create an instance of r with params bound as its type parameters.
func (r *Recursive) WithParams(env TypeEnv, params ...Type) *Recursive {
	next := &Recursive{
		Source:  r,
		Params:  make([]*Var, len(r.Params)),
		Types:   make([]*App, 0, len(r.Types)),
		Names:   r.Names,
		Indexes: r.Indexes,
		Flags:   r.Flags,
		Bind:    r.Bind,
	}
	generic := false
	refs := false
	for i, p := range params {
		p = RealType(p)
		generic = generic || p.IsGeneric()
		refs = refs || p.HasRefs()
		if tv, ok := p.(*Var); ok {
			next.Params[i] = tv
			continue
		}
		tv := env.NewVar(TopLevel)
		tv.SetLink(p)
		next.Params[i] = tv
	}
	next.Bind(next)
	if generic {
		next.Flags |= ContainsGenericVars
	}
	if refs {
		next.Flags |= ContainsRefs
	}
	return next
}

// Recursive link to a type.
type RecursiveLink struct {
	Recursive *Recursive
	Index     int
	// Source which this type was instantiated from, or nil
	Source *RecursiveLink
}

// "Recursive"
func (t *RecursiveLink) TypeName() string { return "RecursiveLink" }

// Check if t contains generic types.
func (t *RecursiveLink) IsGeneric() bool { return t.Recursive.IsGeneric() }

// Check if t contains mutable reference-types.
func (t *RecursiveLink) HasRefs() bool { return t.Recursive.HasRefs() }

// Expand the recursively-linked type for t.
func (t *RecursiveLink) Link() Type { return t.Recursive.Types[t.Index] }

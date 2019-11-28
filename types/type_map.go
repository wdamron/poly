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

import (
	"github.com/benbjohnson/immutable"
)

var emptyMap = immutable.NewSortedMap(nil)

var EmptyTypeMap = TypeMap{emptyMap}

// TypeMap contains immutable mappings from labels to immutable lists of types.
type TypeMap struct {
	m *immutable.SortedMap
}

func NewTypeMap() TypeMap { return TypeMap{emptyMap} }

// Create a TypeMap with a single entry.
func SingletonTypeMap(label string, t Type) TypeMap {
	return TypeMap{emptyMap.Set(label, emptyList.Append(t))}
}

// Create a TypeMap with unscoped labels.
func NewFlatTypeMap(m map[string]Type) TypeMap {
	b := NewTypeMapBuilder()
	for name, t := range m {
		b.Set(name, SingletonTypeList(t))
	}
	return b.Build()
}

// Get the number of entries in the map.
func (m TypeMap) Len() int { return m.m.Len() }

// Get the first entry in the map. Entries are sorted by label.
func (m TypeMap) First() (string, TypeList) {
	if m.Len() == 0 {
		return "", EmptyTypeList
	}
	k, v := m.m.Iterator().Next()
	return k.(string), TypeList{v.(*immutable.List)}
}

// Get the list of types for a label.
func (m TypeMap) Get(label string) (TypeList, bool) {
	l, ok := m.m.Get(label)
	if !ok {
		return TypeList{}, false
	}
	return TypeList{l: l.(*immutable.List)}, true
}

// Iterate over entries in the map.
// If f returns false, iteration will be stopped.
func (m TypeMap) Range(f func(string, TypeList) bool) {
	iter := m.m.Iterator()
	for !iter.Done() {
		k, v := iter.Next()
		if !f(k.(string), TypeList{v.(*immutable.List)}) {
			return
		}
	}
}

// Get an iterator which may be used to read entries in the map, in sequential order.
func (m TypeMap) Iterator() TypeMapIterator {
	return TypeMapIterator{m.m.Iterator()}
}

// Convert the map to a builder for modification, without mutating the existing map.
func (m TypeMap) Builder() TypeMapBuilder {
	imm := m.m
	if imm == nil {
		imm = emptyMap
	}
	return TypeMapBuilder{immutable.NewSortedMapBuilder(imm)}
}

// TypeMapBuilder enables in-place updates of a map before finalization.
type TypeMapBuilder struct {
	b *immutable.SortedMapBuilder
}

func NewTypeMapBuilder() TypeMapBuilder {
	return TypeMapBuilder{immutable.NewSortedMapBuilder(emptyMap)}
}

func (b *TypeMapBuilder) EnsureInitialized() {
	if b.b != nil {
		return
	}
	b.b = immutable.NewSortedMapBuilder(emptyMap)
}

// Get the number of entries in the builder.
func (b TypeMapBuilder) Len() int {
	if b.b == nil {
		return 0
	}
	return b.b.Len()
}

// Set the type list for the given label in the builder.
func (b TypeMapBuilder) Set(label string, ts TypeList) TypeMapBuilder {
	b.b.Set(label, ts.l)
	return b
}

// Delete the given label and corresponding type list from the builder.
func (b TypeMapBuilder) Delete(label string) TypeMapBuilder {
	b.b.Delete(label)
	return b
}

// Finalize the builder into an immutable map.
func (b TypeMapBuilder) Build() TypeMap {
	if b.b == nil {
		return EmptyTypeMap
	}
	return TypeMap{b.b.Map()}
}

// Merge entries into the builder.
func (a TypeMapBuilder) Merge(b TypeMap) TypeMapBuilder {
	b.Range(func(label string, bts TypeList) bool {
		ts, ok := a.b.Get(label)
		if !ok {
			a.Set(label, bts)
			return true
		}
		lb := TypeListBuilder{immutable.NewListBuilder(ts.(*immutable.List))}
		bts.Range(func(i int, t Type) bool {
			lb.Append(t)
			return true
		})
		a.Set(label, lb.Build())
		return true
	})
	return a
}

// TypeMapIterator reads entries in a map, in sequential order.
type TypeMapIterator struct {
	i *immutable.SortedMapIterator
}

// Done returns true if the iterator has reached the end a map.
func (i TypeMapIterator) Done() bool { return i.i.Done() }

// Next advances the iterator and returns the next entry from a map.
func (i TypeMapIterator) Next() (string, TypeList) {
	if i.Done() {
		return "", EmptyTypeList
	}
	k, v := i.i.Next()
	return k.(string), TypeList{v.(*immutable.List)}
}

// Peek returns the next entry from a map without advancing the iterator.
func (i TypeMapIterator) Peek() (string, TypeList) {
	if i.Done() {
		return "", EmptyTypeList
	}
	k, v := i.i.Next()
	i.i.Prev()
	return k.(string), TypeList{v.(*immutable.List)}
}

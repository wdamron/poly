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

var emptyList = immutable.NewList()

var EmptyTypeList = TypeList{emptyList}

// TypeList contains an immutable list of types.
type TypeList struct {
	l *immutable.List
}

func NewTypeList() TypeList { return TypeList{emptyList} }

// Create a TypeList with a single type.
func SingletonTypeList(t Type) TypeList {
	return TypeList{emptyList.Append(t)}
}

func (l TypeList) Len() int                      { return l.l.Len() }
func (l TypeList) Get(i int) Type                { return l.l.Get(i).(Type) }
func (l TypeList) Slice(start, end int) TypeList { return TypeList{l.l.Slice(start, end)} }

// Iterate over entries in the list.
// If f returns false, iteration will be stopped.
func (l TypeList) Range(f func(int, Type) bool) {
	iter := l.l.Iterator()
	for !iter.Done() {
		i, v := iter.Next()
		if !f(i, v.(Type)) {
			return
		}
	}
}

func (l TypeList) Builder() TypeListBuilder {
	imm := l.l
	if imm == nil {
		imm = emptyList
	}
	return TypeListBuilder{immutable.NewListBuilder(imm)}
}

type TypeListBuilder struct {
	b *immutable.ListBuilder
}

func NewTypeListBuilder() TypeListBuilder {
	return TypeListBuilder{immutable.NewListBuilder(emptyList)}
}

func (b *TypeListBuilder) EnsureInitialized() {
	if b.b != nil {
		return
	}
	b.b = immutable.NewListBuilder(emptyList)
}

func (b TypeListBuilder) Len() int {
	if b.b == nil {
		return 0
	}
	return b.b.Len()
}
func (b TypeListBuilder) Append(t Type)     { b.b.Append(t) }
func (b TypeListBuilder) Set(i int, t Type) { b.b.Set(i, t) }
func (b TypeListBuilder) Build() TypeList {
	if b.b == nil {
		return EmptyTypeList
	}
	return TypeList{b.b.List()}
}

package types

import (
	"github.com/benbjohnson/immutable"
)

var emptyList = immutable.NewList()

var EmptyTypeList = TypeList{emptyList}

type TypeList struct {
	l *immutable.List
}

func NewTypeList() TypeList { return TypeList{emptyList} }

func SingletonTypeList(t Type) TypeList {
	return TypeList{emptyList.Append(t)}
}

func (l TypeList) Len() int                      { return l.l.Len() }
func (l TypeList) Get(i int) Type                { return l.l.Get(i).(Type) }
func (l TypeList) Slice(start, end int) TypeList { return TypeList{l.l.Slice(start, end)} }

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

func (b TypeListBuilder) Len() int          { return b.b.Len() }
func (b TypeListBuilder) Append(t Type)     { b.b.Append(t) }
func (b TypeListBuilder) Set(i int, t Type) { b.b.Set(i, t) }
func (b TypeListBuilder) Build() TypeList   { return TypeList{b.b.List()} }

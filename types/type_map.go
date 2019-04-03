package types

import (
	"github.com/benbjohnson/immutable"
)

var emptyMap = immutable.NewSortedMap(nil)

var EmptyTypeMap = TypeMap{emptyMap}

type TypeMap struct {
	m *immutable.SortedMap
}

func NewTypeMap() TypeMap { return TypeMap{emptyMap} }

func SingletonTypeMap(label string, t Type) TypeMap {
	return TypeMap{emptyMap.Set(label, emptyList.Append(t))}
}

func (m TypeMap) Len() int { return m.m.Len() }

func (m TypeMap) First() (string, TypeList) {
	if m.Len() == 0 {
		return "", EmptyTypeList
	}
	k, v := m.m.Iterator().Next()
	return k.(string), TypeList{v.(*immutable.List)}
}

func (m TypeMap) Get(label string) (TypeList, bool) {
	l, ok := m.m.Get(label)
	if !ok {
		return TypeList{}, false
	}
	return TypeList{l: l.(*immutable.List)}, true
}

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

func (m TypeMap) Iterator() TypeMapIterator {
	return TypeMapIterator{m.m.Iterator()}
}

func (m TypeMap) Builder() TypeMapBuilder {
	imm := m.m
	if imm == nil {
		imm = emptyMap
	}
	return TypeMapBuilder{immutable.NewSortedMapBuilder(imm)}
}

type TypeMapBuilder struct {
	b *immutable.SortedMapBuilder
}

func NewTypeMapBuilder() TypeMapBuilder {
	return TypeMapBuilder{immutable.NewSortedMapBuilder(emptyMap)}
}

func (b TypeMapBuilder) Len() int                      { return b.b.Len() }
func (b TypeMapBuilder) Set(label string, ts TypeList) { b.b.Set(label, ts.l) }
func (b TypeMapBuilder) Delete(label string)           { b.b.Delete(label) }
func (b TypeMapBuilder) Build() TypeMap                { return TypeMap{b.b.Map()} }

func (a TypeMapBuilder) Merge(b TypeMap) {
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
}

type TypeMapIterator struct {
	i *immutable.SortedMapIterator
}

func (i TypeMapIterator) Done() bool { return i.i.Done() }

func (i TypeMapIterator) Next() (string, TypeList) {
	if i.Done() {
		return "", EmptyTypeList
	}
	k, v := i.i.Next()
	return k.(string), TypeList{v.(*immutable.List)}
}

func (i TypeMapIterator) Peek() (string, TypeList) {
	if i.Done() {
		return "", EmptyTypeList
	}
	k, v := i.i.Next()
	i.i.Prev()
	return k.(string), TypeList{v.(*immutable.List)}
}

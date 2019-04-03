package poly

import (
	"github.com/benbjohnson/immutable"
	"github.com/wdamron/poly/types"
)

type TypeEnv struct {
	m *immutable.Map
}

func NewTypeEnv() TypeEnv { return TypeEnv{m: emptyMap} }

func (env TypeEnv) Range(f func(string, types.Type) bool) {
	if env.m == nil {
		return
	}
	iter := env.m.Iterator()
	for !iter.Done() {
		k, v := iter.Next()
		if !f(k.(string), v.(types.Type)) {
			return
		}
	}
}

func (env TypeEnv) Builder() TypeEnvBuilder {
	m := env.m
	if m == nil {
		m = emptyMap
	}
	return TypeEnvBuilder{mb: immutable.NewMapBuilder(m)}
}

type TypeEnvBuilder struct {
	mb *immutable.MapBuilder
}

func NewTypeEnvBuilder() TypeEnvBuilder { return TypeEnvBuilder{mb: immutable.NewMapBuilder(emptyMap)} }

func (b TypeEnvBuilder) Set(name string, t types.Type) { b.mb.Set(name, t) }
func (b TypeEnvBuilder) Delete(name string)            { b.mb.Delete(name) }
func (b TypeEnvBuilder) Build() TypeEnv                { return TypeEnv{m: b.mb.Map()} }

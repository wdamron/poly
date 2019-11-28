# package poly

```go
import "github.com/wdamron/poly"
```

`poly` provides inference for a polymorphic type-system with extensible records and variants.

The type-system is an extension of [Hindley-Milner](https://en.wikipedia.org/wiki/Hindley–Milner_type_system) based on Daan Leijen's paper: [Extensible Records with Scoped Labels (Microsoft Research)](https://www.microsoft.com/en-us/research/publication/extensible-records-with-scoped-labels/).

The core of the implementation is based on an [OCaml library](https://github.com/tomprimozic/type-systems/) by Tom Primozic.

## Supported Features

* Extensible records and variants with scoped labels
* Generic type classes, constructor classes, and parametric overloading
* Limited/explicit (type class) subtyping with multiple inheritance
* Mutually-recursive (generic) function expressions within grouped let bindings
* Mutually-recursive (generic) data types
* Transparently aliased (generic) types
* Control-flow graph expressions
* Mutable references with the [value restriction](https://en.wikipedia.org/wiki/Value_restriction)
* Size-bound type variables

## More Info

* Docs (Godoc)
	* [`poly`](https://godoc.org/github.com/wdamron/poly)
	* [`poly/ast`](https://godoc.org/github.com/wdamron/poly/ast)
	* [`poly/types`](https://godoc.org/github.com/wdamron/poly/types)
* [extensible_rows2 (OCaml implementation)](https://github.com/tomprimozic/type-systems/tree/master/extensible_rows2)
* [Extensible Records with Scoped Labels (Leijen, 2005)](https://www.microsoft.com/en-us/research/publication/extensible-records-with-scoped-labels/)
* [Efficient Generalization with Levels (Oleg Kiselyov)](http://okmij.org/ftp/ML/generalization.html#levels)
* [Hindley-Milner type system (Wikipedia)](https://en.wikipedia.org/wiki/Hindley–Milner_type_system)

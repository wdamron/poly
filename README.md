# package poly

```go
import "github.com/wdamron/poly"
```

`poly` provides inference for a polymorphic type-system with extensible records and variants.

The type-system is an extension of [Hindley-Milner](https://en.wikipedia.org/wiki/Hindley–Milner_type_system) based on Daan Leijen's paper: [Extensible Records with Scoped Labels (Microsoft Research)](https://www.microsoft.com/en-us/research/publication/extensible-records-with-scoped-labels/).

The implementation is based on an [OCaml library](https://github.com/tomprimozic/type-systems/) by Tom Primozic.

## More Info

* Docs (Godoc)
	* [`poly`](https://godoc.org/github.com/wdamron/poly)
	* [`poly/ast`](https://godoc.org/github.com/wdamron/poly/ast)
	* [`poly/types`](https://godoc.org/github.com/wdamron/poly/types)
* [extensible_rows2 (OCaml implementation)](https://github.com/tomprimozic/type-systems/tree/master/extensible_rows2)
* [Extensible Records with Scoped Labels (Leijen, 2005)](https://www.microsoft.com/en-us/research/publication/extensible-records-with-scoped-labels/)
* [Efficient Generalization with Levels (Oleg Kiselyov)](http://okmij.org/ftp/ML/generalization.html#levels)
* [Hindley-Milner type system (Wikipedia)](https://en.wikipedia.org/wiki/Hindley–Milner_type_system)

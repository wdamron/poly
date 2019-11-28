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

// poly provides inference for a polymorphic type-system with extensible records and variants.
//
// The type-system is an extension of Hindley-Milner based on Daan Leijen's paper: Extensible Records with Scoped Labels (Microsoft Research).
//
// The implementation is based on an OCaml library by Tom Primozic.
//
//
// Supported Features:
//
//   * Extensible records and variants with scoped labels
//   * Generic type classes, constructor classes, and parametric overloading
//   * Limited/explicit (type class) subtyping with multiple inheritance
//   * Mutually-recursive (generic) function expressions within grouped let bindings
//   * Mutually-recursive (generic) data types
//   * Transparently aliased (generic) types
//   * Control-flow graph expressions
//   * Mutable references with the value restriction
//   * Size-bound type variables
//
//
// Links:
//
// extensible_rows2 (OCaml implementation): https://github.com/tomprimozic/type-systems/tree/master/extensible_rows2
//
// Extensible Records with Scoped Labels (Leijen, 2005): https://www.microsoft.com/en-us/research/publication/extensible-records-with-scoped-labels/
//
// Efficient Generalization with Levels (Oleg Kiselyov): http://okmij.org/ftp/ML/generalization.html#levels
//
// Hindley-Milner type system: https://en.wikipedia.org/wiki/Hindley–Milner_type_system
//
// Value restriction: https://en.wikipedia.org/wiki/Value_restriction
package poly

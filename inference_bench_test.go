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

package poly_test

import (
	"testing"

	. "github.com/wdamron/poly"
	. "github.com/wdamron/poly/construct"

	"github.com/wdamron/poly/ast"
	"github.com/wdamron/poly/types"
)

func BenchmarkMutuallyRecursiveLet(b *testing.B) { // ~10500 ns/op
	env := NewTypeEnv(nil)
	ctx := NewContext()

	env.Declare("add", TArrow2(TConst("int"), TConst("int"), TConst("int")))
	A := env.NewGenericVar()
	env.Declare("if", TArrow3(TConst("bool"), A, A, A))
	env.Declare("somebool", TConst("bool"))

	somebool := Var("somebool")
	add := Var("add")
	f := Var("f")
	g := Var("g")
	h := Var("h")
	id := Var("id")
	x := Var("x")

	expr := LetGroup(
		[]ast.LetBinding{
			{"id", Func1("x", x)},
			{"f", Func1("x", Call(Var("if"), Call(id, somebool), Call(id, x), Call(g, Call(add, x, x))))},
			{"g", Func1("x", Call(Var("if"), somebool, x, Call(id, Call(f, x))))},
		},
		Let("h", Func1("x", Call(id, Call(f, x))),
			RecordExtend(nil,
				LabelValue("f", f),
				LabelValue("g", g),
				LabelValue("h", h),
				LabelValue("id", id))))

	b.ResetTimer()

	for n := 0; n < b.N; n++ {
		ty, err := ctx.Infer(expr, env)
		if err != nil || ty == nil {
			b.Fatal(err)
		}
	}
}

func BenchmarkRecursiveLet(b *testing.B) { // ~1900 ns/op
	env := NewTypeEnv(nil)
	ctx := NewContext()

	env.Declare("add", TArrow2(TConst("int"), TConst("int"), TConst("int")))
	A := env.NewGenericVar()
	env.Declare("if", TArrow3(TConst("bool"), A, A, A))
	env.Declare("somebool", TConst("bool"))

	expr := Func1("x",
		Let("f",
			Func1("x",
				Call(
					Var("if"),
					Var("somebool"),
					Var("x"),
					Call(Var("f"), Call(Var("add"), Var("x"), Var("x"))),
				),
			),
			Call(Var("f"), Var("x")),
		))

	b.ResetTimer()

	for n := 0; n < b.N; n++ {
		ty, err := ctx.Infer(expr, env)
		if err != nil || ty == nil {
			b.Fatal(err)
		}
	}
}

func BenchmarkInstanceLookups(t *testing.B) { // ~5100 ns/op
	env := NewTypeEnv(nil)
	ctx := NewContext()

	Functor, err := env.DeclareTypeClass("Functor", func(f *types.Var) types.MethodSet {
		f.SetWeak()
		a, b := env.NewGenericVar(), env.NewGenericVar()
		return types.MethodSet{
			"fmap": TArrow2(TArrow1(a, b), TApp(f, a), TApp(f, b)),
		}
	})
	if err != nil {
		t.Fatal(err)
	}

	Applicative, err := env.DeclareTypeClass("Applicative", func(f *types.Var) types.MethodSet {
		f.SetWeak()
		a, b := env.NewGenericVar(), env.NewGenericVar()
		return types.MethodSet{
			"pure":  TArrow1(a, TApp(f, a)),
			"(<*>)": TArrow2(TApp(f, TArrow1(a, b)), TApp(f, a), TApp(f, b)),
		}
	}, Functor) // extends/subsumes Functor
	if err != nil {
		t.Fatal(err)
	}

	Monad, err := env.DeclareTypeClass("Monad", func(m *types.Var) types.MethodSet {
		m.SetWeak()
		a, b := env.NewGenericVar(), env.NewGenericVar()
		return types.MethodSet{
			"(>>=)": TArrow2(TApp(m, a), TArrow1(a, TApp(m, b)), TApp(m, b)),
		}
	}, Applicative) // extends/subsumes Applicative
	if err != nil {
		t.Fatal(err)
	}

	a, b, option := env.NewGenericVar(), env.NewGenericVar(), TConst("Option")
	env.Declare("option_map", TArrow2(TArrow1(a, b), TApp(option, a), TApp(option, b)))
	a = env.NewGenericVar()
	env.Declare("some", TArrow1(a, TApp(option, a)))
	a, b = env.NewGenericVar(), env.NewGenericVar()
	env.Declare("option_ap", TArrow2(TApp(option, TArrow1(a, b)), TApp(option, a), TApp(option, b)))
	a, b = env.NewGenericVar(), env.NewGenericVar()
	env.Declare("option_bind", TArrow2(TApp(option, a), TArrow1(a, TApp(option, b)), TApp(option, b)))
	optionMethods := map[string]string{"fmap": "option_map", "pure": "some", "(<*>)": "option_ap", "(>>=)": "option_bind"}
	if _, err := env.DeclareInstance(Monad, option, optionMethods); err != nil {
		t.Fatal(err)
	}

	env.Declare("itoa", TArrow1(TConst("int"), TConst("string")))
	env.Declare("someintoption", TApp(option, TConst("int")))

	t.ResetTimer()

	for n := 0; n < t.N; n++ {
		expr := Call(Var("(>>=)"), Var("someintoption"), Func1("i", Call(Var("pure"), Call(Var("itoa"), Var("i")))))
		ty, err := ctx.Infer(expr, env)
		if err != nil || ty == nil {
			t.Fatal(err)
		}

		expr = Call(Var("fmap"), Var("itoa"), Var("someintoption"))
		ty, err = ctx.Infer(expr, env)
		if err != nil || ty == nil {
			t.Fatal(err)
		}
	}
}

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

func mustInfer(t *testing.T, env *TypeEnv, ctx *InferenceContext, expr ast.Expr, typeString string) {
	ty, err := ctx.Infer(expr, env)
	if err != nil {
		t.Fatal(err)
	}
	if types.TypeString(ty) != typeString {
		t.Fatalf("type: %s", types.TypeString(ty))
	}
}

func TestLiterals(t *testing.T) {
	env := NewTypeEnv(nil)
	ctx := NewContext()

	env.Declare("x", TConst("int"))

	xvec := &ast.Literal{
		Syntax: "[x]", Using: []string{"x"},
		Construct: func(env types.TypeEnv, level int, using []types.Type) (types.Type, error) {
			return TApp(TConst("vec"), using[0]), nil // x :: int |- vec[int]
		},
	}
	mustInfer(t, env, ctx, Let("vec", xvec, Var("vec")), "vec[int]")
}

func TestPipes(t *testing.T) {
	env := NewTypeEnv(nil)
	ctx := NewContext()

	env.Declare("x", TConst("int"))
	env.Declare("id", TArrow1(TConst("int"), TConst("int")))
	env.Declare("add", TArrow2(TConst("int"), TConst("int"), TConst("int")))
	env.Declare("itoa", TArrow1(TConst("int"), TConst("string")))

	expr := Pipe("$", Var("x"),
		Call(Var("itoa"), Var("$")))

	mustInfer(t, env, ctx, expr, "string")

	expr = Pipe("$", Var("x"),
		Call(Var("id"), Var("$")),
		Call(Var("itoa"), Var("$")))

	mustInfer(t, env, ctx, expr, "string")

	expr = Pipe("$", Var("x"),
		Call(Var("id"), Var("$")),
		Call(Var("add"), Var("$"), Var("$")),
		Call(Var("itoa"), Var("$")))

	if ast.ExprString(expr) != "pipe $ = x |> id($) |> add($, $) |> itoa($)" {
		t.Fatalf("expr: %s", ast.ExprString(expr))
	}
	mustInfer(t, env, ctx, expr, "string")
}

func TestSizes(t *testing.T) {
	env := NewTypeEnv(nil)
	ctx := NewContext()

	intarray := TApp(TConst("array"), TConst("int"), env.NewGenericSize())

	env.Declare("xs", TApp(TConst("array"), TConst("int"), TSize(8)))
	env.Declare("ys", TApp(TConst("array"), TConst("int"), TSize(16)))
	env.Declare("zs", TApp(TConst("array"), TConst("int"), TConst("foo")))
	env.Declare("add", TArrow2(intarray, intarray, intarray))

	mustInfer(t, env, ctx, Var("add"), "size 'a => (array[int, 'a], array[int, 'a]) -> array[int, 'a]")
	mustInfer(t, env, ctx, Call(Var("add"), Var("xs"), Var("xs")), "array[int, 8]")
	mustInfer(t, env, ctx, Call(Var("add"), Var("ys"), Var("ys")), "array[int, 16]")
	if _, err := ctx.Infer(Call(Var("add"), Var("xs"), Var("ys")), env); err == nil {
		t.Fatalf("Expected size-mismatch error")
	}
	if _, err := ctx.Infer(Call(Var("add"), Var("zs"), Var("zs")), env); err == nil {
		t.Fatalf("Expected size type-restriction error")
	}
}

func TestRefs(t *testing.T) {
	env := NewTypeEnv(nil)
	ctx := NewContext()

	a := env.NewVar(types.TopLevel)
	env.DeclareWeak("r", TRef(a))
	mustInfer(t, env, ctx, Var("r"), "weak '_0 => ref['_0]")

	a = env.NewGenericVar()
	env.Declare("new", TArrow(nil, TRef(a)))
	expr := RecordExtend(RecordEmpty(), LabelValue("id", Func1("x", Var("x"))), LabelValue("r", Call(Var("new"))))
	mustInfer(t, env, ctx, expr, "weak '_3 => {id : 'a -> 'a, r : ref['_3]}")
	ty, _ := ctx.Infer(expr, env)
	if !ty.HasRefs() {
		t.Fatalf("type contains a reference")
	}
	if !ty.IsGeneric() {
		t.Fatalf("type contains a generic type-variable")
	}
}

func TestRecords(t *testing.T) {
	env := NewTypeEnv(nil)
	ctx := NewContext()

	env.Declare("a", TConst("A"))
	env.Declare("b", TConst("B"))

	record := RecordExtend(nil, LabelValue("a", Var("a")), LabelValue("b", Var("b")))
	mustInfer(t, env, ctx, RecordSelect(record, "a"), "A")
	mustInfer(t, env, ctx, RecordRestrict(record, "a"), "{b : B}")
}

func TestAliases(t *testing.T) {
	env := NewTypeEnv(nil)
	ctx := NewContext()

	a := env.NewGenericVar()
	sliceHeader := TRecord(TRowExtend(types.RowEmptyPointer, TypeMap(map[string]types.Type{
		"data": TRef(TApp(TConst("array"), a)),
		"len":  TConst("int"),
		"cap":  TConst("int"),
	})))
	slice := types.Generalize(TAlias(TApp(TConst("slice"), a), sliceHeader))

	intSliceHeader := TRecord(TRowExtend(types.RowEmptyPointer, TypeMap(map[string]types.Type{
		"data": TRef(TApp(TConst("array"), TConst("int"))),
		"len":  TConst("int"),
		"cap":  TConst("int"),
	})))
	intSlice := TAlias(TApp(TConst("slice"), TConst("int")), intSliceHeader)

	env.Declare("append", TArrow2(slice, slice, slice))
	env.Declare("someints", intSliceHeader)
	env.Declare("someints2", intSlice)
	env.Declare("someints3", TApp(TConst("slice"), TConst("int")))

	mustInfer(t, env, ctx, Var("someints"), "{cap : int, data : ref[array[int]], len : int}")
	mustInfer(t, env, ctx, Var("someints2"), "slice[int]")

	sel := Let("f", Func1("x", RecordSelect(Var("x"), "data")),
		Call(Var("f"), Var("someints2")))

	mustInfer(t, env, ctx, sel, "ref[array[int]]")
	mustInfer(t, env, ctx, Call(Var("append"), Var("someints"), Var("someints")), "slice[int]")
	mustInfer(t, env, ctx, Call(Var("append"), Var("someints"), Var("someints2")), "slice[int]")
	mustInfer(t, env, ctx, Call(Var("append"), Var("someints2"), Var("someints3")), "slice[int]")
}

func TestRecursiveTypes(t *testing.T) {
	env := NewTypeEnv(nil)
	ctx := NewContext()

	params := []*types.Var{env.NewGenericVar()}
	list := env.NewSimpleRecursive(params, func(rec *types.Recursive, self *types.RecursiveLink) {
		a := rec.Params[0]
		rec.AddType("list", TAlias(TApp(TConst("list"), a),
			TRecordFlat(map[string]types.Type{"head": a, "tail": self})))
	})

	env.Declare("someintlist", list.WithParams(env, TConst("int")).GetType("list"))

	var expr ast.Expr = RecordSelect(RecordSelect(RecordSelect(Var("someintlist"), "tail"), "tail"), "tail")
	mustInfer(t, env, ctx, expr, "list[int]")

	expr = RecordSelect(RecordSelect(RecordSelect(Var("someintlist"), "tail"), "tail"), "head")
	mustInfer(t, env, ctx, expr, "int")

	a, b := env.NewGenericVar(), env.NewGenericVar()
	listA, listB := list.WithParams(env, a).GetType("list"), list.WithParams(env, b).GetType("list")
	env.Declare("list_map", TArrow2(listA, TArrow1(a, b), listB))
	env.Declare("itoa", TArrow1(TConst("int"), TConst("string")))
	env.Declare("atoi", TArrow1(TConst("string"), TConst("int")))

	expr = RecordSelect(RecordSelect(Call(Var("list_map"), Var("someintlist"), Var("itoa")), "tail"), "tail")
	mustInfer(t, env, ctx, expr, "list[string]")

	expr = RecordSelect(RecordSelect(Call(Var("list_map"), Var("someintlist"), Var("itoa")), "tail"), "head")
	mustInfer(t, env, ctx, expr, "string")

	expr = Pipe("$", Var("someintlist"),
		Call(Var("list_map"), Var("$"), Var("itoa")),
		RecordSelect(Var("$"), "tail"),
		Call(Var("list_map"), Var("$"), Var("atoi")),
		RecordSelect(Var("$"), "tail"),
		Call(Var("list_map"), Var("$"), Var("itoa")),
		RecordSelect(Var("$"), "head"))

	mustInfer(t, env, ctx, expr, "string")
}

func TestMutuallyRecursiveTypes(t *testing.T) {
	env := NewTypeEnv(nil)
	ctx := NewContext()

	cycle := env.NewRecursive(nil, func(rec *types.Recursive) {
		int2bool := TApp(TConst("cycle"), TConst("int"), TConst("bool"))
		bool2int := TApp(TConst("cycle"), TConst("bool"), TConst("int"))
		rec.AddType("int2bool", int2bool)
		rec.AddType("bool2int", bool2int)
		int2bool.Underlying = TRecordFlat(map[string]types.Type{
			"v": TConst("int"), "link": TRecursiveLink(rec, "bool2int"),
		})
		bool2int.Underlying = TRecordFlat(map[string]types.Type{
			"v": TConst("bool"), "link": TRecursiveLink(rec, "int2bool"),
		})
	})

	int2bool := cycle.GetType("int2bool")
	env.Declare("someint2bool", int2bool)

	var expr ast.Expr = Var("someint2bool")
	mustInfer(t, env, ctx, expr, "cycle[int, bool]")

	expr = Let("f", Func1("x", RecordSelect(Var("x"), "link")),
		Call(Var("f"), Var("someint2bool")))

	mustInfer(t, env, ctx, expr, "cycle[bool, int]")

	expr = Let("f", Func1("x", RecordSelect(Var("x"), "v")),
		Call(Var("f"), Var("someint2bool")))

	mustInfer(t, env, ctx, expr, "int")

	expr = Let("link", RecordSelect(Var("someint2bool"), "link"),
		RecordSelect(Var("link"), "link"))

	mustInfer(t, env, ctx, expr, "cycle[int, bool]")

	expr = Pipe("$", Var("someint2bool"),
		RecordSelect(Var("$"), "link"),
		RecordSelect(Var("$"), "link"),
		RecordSelect(Var("$"), "link"),
		RecordSelect(Var("$"), "v"))

	mustInfer(t, env, ctx, expr, "bool")
}

func TestGenericMutuallyRecursiveTypes(t *testing.T) {
	env := NewTypeEnv(nil)
	ctx := NewContext()

	params := []*types.Var{env.NewGenericVar(), env.NewGenericVar()}
	cycle := env.NewRecursive(params, func(rec *types.Recursive) {
		a, b := rec.Params[0], rec.Params[1]
		a2b := TApp(TConst("cycle"), a, b)
		b2a := TApp(TConst("cycle"), b, a)
		rec.AddType("a2b", a2b)
		rec.AddType("b2a", b2a)
		a2b.Underlying = TRecordFlat(map[string]types.Type{
			"v": a, "link": TRecursiveLink(rec, "b2a"),
		})
		b2a.Underlying = TRecordFlat(map[string]types.Type{
			"v": b, "link": TRecursiveLink(rec, "a2b"),
		})
	})

	intboolcycle := cycle.WithParams(env, TConst("int"), TConst("bool"))
	env.Declare("someint2bool", intboolcycle.GetType("a2b"))

	expr := Pipe("$", Var("someint2bool"),
		RecordSelect(Var("$"), "link"),
		RecordSelect(Var("$"), "link"),
		RecordSelect(Var("$"), "link"))

	mustInfer(t, env, ctx, expr, "cycle[bool, int]")
}

func TestRecursiveLet(t *testing.T) {
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

	if ast.ExprString(expr) != "fn (x) -> let f(x) = if(somebool, x, f(add(x, x))) in f(x)" {
		t.Fatalf("expr: %s", ast.ExprString(expr))
	}
	mustInfer(t, env, ctx, expr, "int -> int")
}

func TestMutuallyRecursiveLet(t *testing.T) {
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

	expect := "" +
		"let id(x) = x" +
		" and f(x) = if(id(somebool), id(x), g(add(x, x)))" +
		" and g(x) = if(somebool, x, id(f(x)))" +
		" in" +
		" let h(x) = id(f(x))" +
		" in {f = f, g = g, h = h, id = id}"

	if ast.ExprString(expr) != expect {
		t.Fatalf("expr: %s", ast.ExprString(expr))
	}
	mustInfer(t, env, ctx, expr, "{f : int -> int, g : int -> int, h : int -> int, id : 'a -> 'a}")
}

func TestVariantMatch(t *testing.T) {
	env := NewTypeEnv(nil)
	ctx := NewContext()

	env.Declare("add", TArrow2(TConst("int"), TConst("int"), TConst("int")))
	a := env.NewGenericVar()
	env.Declare("if", TArrow3(TConst("bool"), a, a, a))
	env.Declare("somebool", TConst("bool"))
	env.Declare("someint", TConst("int"))
	env.Declare("somebool", TConst("bool"))

	ifExpr := Call(Var("if"), Var("somebool"), Variant("i", Var("someint")), Variant("b", Var("somebool")))
	mustInfer(t, env, ctx, ifExpr, "[b : bool, i : int | 'a]")

	matchExpr := Match(ifExpr, // [i : int, b : bool | 'a]
		[]ast.MatchCase{
			{Label: "i", Var: "i", Value: Call(Var("add"), Var("i"), Var("i"))},
			{Label: "b", Var: "b", Value: Call(Var("if"), Var("b"), Var("someint"), Var("someint"))},
		},
		// default case:
		nil)

	mustInfer(t, env, ctx, matchExpr, "int")

	matchExpr = Match(ifExpr, // [i : int, b : bool | 'a]
		[]ast.MatchCase{
			{Label: "i", Var: "i", Value: Call(Var("add"), Var("i"), Var("i"))},
			// does not match variant type for the match argument (bool != int):
			{Label: "b", Var: "b", Value: Call(Var("add"), Var("b"), Var("b"))},
		},
		// default case:
		nil)

	if _, err := ctx.Infer(matchExpr, env); err == nil {
		t.Fatalf("expected error due to invalid variant argument for match")
	}

	expr := Let("f", Func1("x", Match(Var("x"),
		[]ast.MatchCase{
			{Label: "a", Var: "i", Value: Call(Var("f"), Variant("b", Var("i")))},
			{Label: "b", Var: "i", Value: Call(Var("f"), Variant("c", Var("i")))},
			{Label: "c", Var: "i", Value: Call(Var("add"), Var("i"), Var("someint"))},
		},
		// default case:
		&ast.MatchCase{Var: "_", Value: Var("someint")})),
		// let-in:
		Var("f"))

	if ast.ExprString(expr) != "let f(x) = match x { :a i -> f(:b i) | :b i -> f(:c i) | :c i -> add(i, someint) | _ -> someint } in f" {
		t.Fatalf("expr: %s", ast.ExprString(expr))
	}
	mustInfer(t, env, ctx, expr, "[a : int, b : int, c : int | 'a] -> int")

	fnExpr := Func2("x", "y",
		Match(
			Var("x"),
			[]ast.MatchCase{
				{Label: "a", Var: "i", Value: Call(Var("add"), Var("i"), Var("i"))},
				{Label: "b", Var: "i", Value: Call(Var("add"), Var("i"), Var("i"))},
				{Label: "c", Var: "_", Value: Var("y")},
			},
			nil,
		))

	if ast.ExprString(fnExpr) != "fn (x, y) -> match x { :a i -> add(i, i) | :b i -> add(i, i) | :c _ -> y }" {
		t.Fatalf("expr: %s", ast.ExprString(fnExpr))
	}
	mustInfer(t, env, ctx, fnExpr, "([a : int, b : int, c : 'a], int) -> int")

	// Call:

	callExpr := Call(fnExpr, Variant("c", Var("somebool")), Var("someint"))

	if ast.ExprString(callExpr) != "(fn (x, y) -> match x { :a i -> add(i, i) | :b i -> add(i, i) | :c _ -> y })(:c somebool, someint)" {
		t.Fatalf("expr: %s", ast.ExprString(callExpr))
	}
	mustInfer(t, env, ctx, callExpr, "int")
}

func TestSafeStacks(t *testing.T) {
	env := NewTypeEnv(nil)
	ctx := NewContext()

	// Use scoped labels within records to model a type-safe (polymorphic) stack
	// with static underflow checking:

	rest, a := env.NewGenericVar(), env.NewGenericVar()
	env.Declare("push", TArrow2( // ({'rest}, 'a) -> {top : 'a | 'rest}
		TRecord(rest), a,
		TRecord(TRowExtend(rest, TypeMap(map[string]types.Type{"top": a})))))

	rest, a = env.NewGenericVar(), env.NewGenericVar()
	env.Declare("peek", TArrow1( // {top : 'a | 'rest} -> 'a
		TRecord(TRowExtend(rest, TypeMap(map[string]types.Type{"top": a}))),
		a))

	rest, a = env.NewGenericVar(), env.NewGenericVar()
	env.Declare("pop", TArrow1( // {top : 'a | 'rest} -> 'rest
		TRecord(TRowExtend(rest, TypeMap(map[string]types.Type{"top": a}))),
		TRecord(rest)))

	env.Declare("i", TConst("int"))
	env.Declare("b", TConst("bool"))

	expr := Let("stack", RecordEmpty(),
		Let("stack", Call(Var("push"), Var("stack"), Var("i")), // i
			Let("stack", Call(Var("push"), Var("stack"), Var("b")), // i b
				Let("top", Call(Var("peek"), Var("stack")), // i b
					Let("stack", Call(Var("pop"), Var("stack")), // i
						Var("top"))))))

	mustInfer(t, env, ctx, expr, "bool")

	expr = Let("stack", RecordEmpty(),
		Let("stack", Call(Var("push"), Var("stack"), Var("i")), // i
			Let("stack", Call(Var("push"), Var("stack"), Var("b")), // i b
				Let("stack", Call(Var("pop"), Var("stack")), // i
					Let("top", Call(Var("peek"), Var("stack")), // i
						Let("stack", Call(Var("pop"), Var("stack")), // empty
							Var("top")))))))

	mustInfer(t, env, ctx, expr, "int")

	expr = Let("stack", RecordEmpty(),
		Let("stack", Call(Var("push"), Var("stack"), Var("i")), // i
			Let("stack", Call(Var("push"), Var("stack"), Var("b")), // i b
				Let("stack", Call(Var("pop"), Var("stack")), // i
					Let("stack", Call(Var("pop"), Var("stack")), // empty
						Let("stack", Call(Var("pop"), Var("stack")), // underflow
							Var("stack")))))))

	_, err := ctx.Infer(expr, env)
	if err == nil {
		t.Fatalf("expected stack underflow error")
	}
}

func TestHigherKindedTypes(t *testing.T) {
	env := NewTypeEnv(nil)
	ctx := NewContext()

	// class Functor 'f where
	//   fmap :: (('a -> 'b), 'f['a]) -> 'f['b]
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

	// class (Functor 'f) => Applicative 'f where
	//   pure  :: 'a -> 'f['a]
	//   (<*>) :: ('f[('a -> 'b)], 'f['a]) -> 'f['b]
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

	// class (Applicative 'm) => Monad 'm where
	//   (>>=) :: ('m['a], (a -> 'm['b])) -> 'm['b]
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

	// fmap       :: (('a -> 'b),     'f['a]) ->     'f['b]
	// option_map :: (('a -> 'b), Option['a]) -> Option['b]
	//
	// pure :: 'a ->     'f['a]
	// some :: 'a -> Option['a]
	//
	// (<*>)     ::     ('f[('a -> 'b)],     'f['a]) ->     'f['b]
	// option_ap :: (Option[('a -> 'b)], Option['a]) -> Option['b]
	//
	// (>>=)       ::     ('m['a], (a ->     'm['b])) ->     'm['b]
	// option_bind :: (Option['a], (a -> Option['b])) -> Option['b]
	//
	// instance Monad Option where
	//   fmap  = option_map
	//   pure  = some
	//   (<*>) = option_ap
	//   (>>=) = option_bind
	a, b, option := env.NewGenericVar(), env.NewGenericVar(), TConst("Option")
	env.Declare("option_map", TArrow2(TArrow1(a, b), TApp(option, a), TApp(option, b)))
	a = env.NewGenericVar()
	env.Declare("some", TArrow1(a, TApp(option, a)))
	a, b = env.NewGenericVar(), env.NewGenericVar()
	env.Declare("option_ap", TArrow2(TApp(option, TArrow1(a, b)), TApp(option, a), TApp(option, b)))
	a, b = env.NewGenericVar(), env.NewGenericVar()
	env.Declare("option_bind", TArrow2(TApp(option, a), TArrow1(a, TApp(option, b)), TApp(option, b)))
	optionMethods := map[string]string{"fmap": "option_map", "pure": "some", "(<*>)": "option_ap", "(>>=)": "option_bind"}
	// Declaring instances for Functor and Applicative is not strictly necessary, but should not cause errors
	// due to overlapping instances:
	if _, err := env.DeclareInstance(Functor, option, optionMethods); err != nil {
		t.Fatal(err)
	}
	if _, err := env.DeclareInstance(Applicative, option, optionMethods); err != nil {
		t.Fatal(err)
	}
	if _, err := env.DeclareInstance(Monad, option, optionMethods); err != nil {
		t.Fatal(err)
	}

	env.Declare("itoa", TArrow1(TConst("int"), TConst("string")))
	env.Declare("someintoption", TApp(option, TConst("int")))

	var expr ast.Expr = Call(Var("fmap"), Var("itoa"), Var("someintoption"))
	mustInfer(t, env, ctx, expr, "Option[string]")

	expr = Call(Var("(>>=)"), Var("someintoption"), Func1("i", Call(Var("pure"), Call(Var("itoa"), Var("i")))))
	if ast.ExprString(expr) != "(>>=)(someintoption, fn (i) -> pure(itoa(i)))" {
		t.Fatalf("expr: %s", ast.ExprString(expr))
	}
	mustInfer(t, env, ctx, expr, "Option[string]")

	// fmap    :: (('a -> 'b),  'f['a]) ->  'f['b]
	// ref_map :: (('a -> 'b), ref['a]) -> ref['b]
	//
	// instance Functor ref where
	//   fmap = ref_map
	a, b = env.NewGenericVar(), env.NewGenericVar()
	env.Declare("ref_map", TArrow2(TArrow1(a, b), TRef(a), TRef(b)))
	if _, err := env.DeclareInstance(Functor, types.RefType, map[string]string{"fmap": "ref_map"}); err != nil {
		t.Fatal(err)
	}
	env.Declare("someintref", TRef(TConst("int")))
	expr = Call(Var("fmap"), Var("itoa"), Var("someintref"))
	mustInfer(t, env, ctx, expr, "ref[string]")

	// detect invalid instances

	// fmap    :: (('a -> 'b), 'f['a]) -> 'f['b]
	// bad_map :: (('a -> 'b),  A['a]) ->  B['b]
	//                          ^----------^
	//
	// instance Functor A where
	//   fmap = bad_map
	a, b = env.NewGenericVar(), env.NewGenericVar()
	env.Declare("bad_map", TArrow2(TArrow1(a, b), TApp(TConst("A"), a), TApp(TConst("B"), b)))
	if _, err = env.DeclareInstance(Functor, TConst("A"), map[string]string{"fmap": "bad_map"}); err == nil {
		t.Fatalf("expected invalid-instance error")
	}

	// bad applicative (Vec is not declared as a Functor instance)
	a = env.NewGenericVar()
	env.Declare("new_vec", TArrow1(a, TApp(TConst("Vec"), a)))
	a, b = env.NewGenericVar(), env.NewGenericVar()
	env.Declare("vec_ap", TArrow2(TApp(TConst("Vec"), TArrow1(a, b)), TApp(TConst("Vec"), a), TApp(TConst("Vec"), b)))
	if _, err := env.DeclareInstance(Applicative, TConst("Vec"), map[string]string{"pure": "new_vec", "(<*>)": "vec_ap"}); err == nil {
		t.Fatalf("expected invalid-instance error")
	}
}

func TestConstraints(t *testing.T) {
	env := NewTypeEnv(nil)
	ctx := NewContext()

	// type hierarchy

	intType := TConst("int")
	shortType := TConst("short")
	floatType := TConst("float")
	boolType := TConst("bool")

	intVecType := TApp(TConst("vec"), intType)

	Add, err := env.DeclareTypeClass("Add", func(param *types.Var) types.MethodSet {
		return types.MethodSet{
			"+": TArrow2(param, param, param),
		}
	})
	if err != nil {
		t.Fatal(err)
	}
	adderVecType := TApp(TConst("vec"), env.NewQualifiedVar(types.InstanceConstraint{Add}))
	if types.TypeString(adderVecType) != "Add 'a => vec['a]" {
		t.Fatalf("invalid vec string: %s", types.TypeString(adderVecType))
	}

	Int, err := env.DeclareTypeClass("Int", func(param *types.Var) types.MethodSet {
		return types.MethodSet{
			"&": TArrow2(param, param, param),
		}
	}, Add) // implements/subsumes Add
	if err != nil {
		t.Fatal(err)
	}

	env.Declare("int_add", TArrow2(intType, intType, intType))
	env.Declare("int_and", TArrow2(intType, intType, intType))
	env.Declare("short_add", TArrow2(shortType, shortType, shortType))
	env.Declare("short_and", TArrow2(shortType, shortType, shortType))
	env.Declare("float_add", TArrow2(floatType, floatType, floatType))
	env.Declare("vec_add", TArrow2(adderVecType, adderVecType, adderVecType))
	env.Declare("int_vec_add", TArrow2(intVecType, intVecType, intVecType))
	env.Declare("bad_bool_add", TArrow2(boolType, intType, boolType))

	if _, err := env.DeclareInstance(Int, intType, map[string]string{"+": "int_add", "&": "int_and"}); err != nil {
		t.Fatal(err)
	}
	if _, err := env.DeclareInstance(Int, shortType, map[string]string{"+": "short_add", "&": "short_and"}); err != nil {
		t.Fatal(err)
	}
	if _, err := env.DeclareInstance(Add, floatType, map[string]string{"+": "float_add"}); err != nil {
		t.Fatal(err)
	}
	if _, err := env.DeclareInstance(Add, adderVecType, map[string]string{"+": "vec_add"}); err != nil {
		t.Fatal(err)
	}
	// detect overlapping instances:
	if _, err := env.DeclareInstance(Add, intVecType, map[string]string{"+": "int_vec_add"}); err == nil {
		t.Fatalf("expected instance-overlap error")
	}

	env.Declare("somebool", boolType)
	env.Declare("someint", intType)
	env.Declare("someshort", shortType)
	env.Declare("somefloat", floatType)
	env.Declare("someintvec", TApp(TConst("vec"), intType))
	env.Declare("somefloatvec", TApp(TConst("vec"), floatType))
	env.Declare("someboolvec", TApp(TConst("vec"), boolType))

	// unions

	ABC, err := env.DeclareUnionTypeClass("ABC", nil, TConst("A"), TConst("B"), TConst("C"))
	if err != nil {
		t.Fatal(err)
	}
	abc := env.NewQualifiedVar(types.InstanceConstraint{ABC})
	env.Declare("fabc", TArrow1(abc, abc))
	env.Declare("somea", TConst("A"))

	call := Call(Var("fabc"), Var("somea"))
	mustInfer(t, env, ctx, call, "A")

	// method-instance lookups

	call = Call(Var("+"), Var("someint"), Var("someint"))
	if err = ctx.AnnotateDirect(call, env); err != nil {
		t.Fatal(err)
	}
	if types.TypeString(call.Type()) != "int" {
		t.Fatalf("expected int return, found " + types.TypeString(call.Type()))
	}

	method := call.FuncType().Method
	if method == nil || method.Name != "+" || method.TypeClass != Add {
		t.Fatalf("no method annotated on call or method = %#+v", call.FuncType())
	}
	methodInstance := env.FindMethodInstance(call.FuncType())
	if methodInstance == nil {
		t.Fatal("no method instance found")
	}
	if types.TypeString(methodInstance.Param) != "int" {
		t.Fatalf("expected int instance, found %s", types.TypeString(methodInstance.Param))
	}
	if methodInstance.MethodNames["+"] != "int_add" {
		t.Fatalf("expected int_add, found %s", methodInstance.MethodNames["+"])
	}

	// method/instance inference

	var addWrapper ast.Expr = Func2("x", "y", Call(Var("+"), Var("x"), Var("y")))
	mustInfer(t, env, ctx, addWrapper, "Add 'a => ('a, 'a) -> 'a")

	var expr ast.Expr = Call(addWrapper, Var("someint"), Var("someint"))
	mustInfer(t, env, ctx, expr, "int")

	expr = Call(addWrapper, Var("someshort"), Var("someshort"))
	mustInfer(t, env, ctx, expr, "short")

	expr = Call(addWrapper, Var("somefloat"), Var("somefloat"))
	mustInfer(t, env, ctx, expr, "float")

	expr = Call(addWrapper, Var("someint"), Var("somefloat"))
	if _, err := ctx.Infer(expr, env); err == nil {
		t.Fatalf("expected constraint error")
	}

	expr = Call(addWrapper, Var("somebool"), Var("somebool"))
	if _, err := ctx.Infer(expr, env); err == nil {
		t.Fatalf("expected constraint error")
	}

	expr = Call(addWrapper, Var("someintvec"), Var("someintvec"))
	mustInfer(t, env, ctx, expr, "vec[int]")

	expr = Call(addWrapper, Var("somefloatvec"), Var("somefloatvec"))
	mustInfer(t, env, ctx, expr, "vec[float]")

	expr = Call(addWrapper, Var("someboolvec"), Var("someboolvec"))
	if _, err := ctx.Infer(expr, env); err == nil {
		t.Fatalf("expected constraint error")
	}

	expr = Call(Var("&"), Var("someint"), Var("someint"))
	mustInfer(t, env, ctx, expr, "int")

	expr = Call(Var("&"), Var("someshort"), Var("someshort"))
	mustInfer(t, env, ctx, expr, "short")

	expr = Call(Var("&"), Var("somebool"), Var("somebool"))
	if _, err := ctx.Infer(expr, env); err == nil {
		t.Fatalf("expected constraint error")
	}

	expr = Call(Var("&"), Var("someint"), Var("someshort"))
	_, err = ctx.Infer(expr, env)
	if _, err := ctx.Infer(expr, env); err == nil {
		t.Fatalf("expected constraint error")
	}

	addWrapper = Func1("x", Call(Var("+"), Var("x"), Var("someint")))
	mustInfer(t, env, ctx, addWrapper, "int -> int")

	// instance validation

	if _, err := env.DeclareInstance(Int, boolType, map[string]string{"&": "bool_and"}); err == nil {
		t.Fatalf("expected missing-method error")
	}
	if _, err := env.DeclareInstance(Add, boolType, map[string]string{"+": "bad_bool_add"}); err == nil {
		t.Fatalf("expected invalid-method error")
	}
}

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

	ty, err := ctx.Infer(Let("vec", xvec, Var("vec")), env)
	if err != nil {
		t.Fatal(err)
	}
	if types.TypeString(ty) != "vec[int]" {
		t.Fatalf("type: %s", types.TypeString(ty))
	}
}

func TestSizes(t *testing.T) {
	env := NewTypeEnv(nil)
	ctx := NewContext()

	intarray := TApp(TConst("array"), TConst("int"), env.NewGenericSize())

	env.Declare("xs", TApp(TConst("array"), TConst("int"), TSize(8)))
	env.Declare("ys", TApp(TConst("array"), TConst("int"), TSize(16)))
	env.Declare("zs", TApp(TConst("array"), TConst("int"), TConst("foo")))

	env.Declare("add", TArrow2(intarray, intarray, intarray))

	ty, err := ctx.Infer(Var("add"), env)
	if err != nil {
		t.Fatal(err)
	}
	if types.TypeString(ty) != "size 'a => (array[int, 'a], array[int, 'a]) -> array[int, 'a]" {
		t.Fatalf("type: %s", types.TypeString(ty))
	}

	ty, err = ctx.Infer(Call(Var("add"), Var("xs"), Var("xs")), env)
	if err != nil {
		t.Fatal(err)
	}
	if types.TypeString(ty) != "array[int, 8]" {
		t.Fatalf("type: %s", types.TypeString(ty))
	}

	ty, err = ctx.Infer(Call(Var("add"), Var("ys"), Var("ys")), env)
	if err != nil {
		t.Fatal(err)
	}
	if types.TypeString(ty) != "array[int, 16]" {
		t.Fatalf("type: %s", types.TypeString(ty))
	}

	if _, err = ctx.Infer(Call(Var("add"), Var("xs"), Var("ys")), env); err == nil {
		t.Fatalf("Expected size-mismatch error")
	}

	if _, err = ctx.Infer(Call(Var("add"), Var("zs"), Var("zs")), env); err == nil {
		t.Fatalf("Expected size type-restriction error")
	}
}

func TestRefs(t *testing.T) {
	env := NewTypeEnv(nil)
	ctx := NewContext()

	a := env.NewVar(types.TopLevel)
	env.DeclareWeak("r", TRef(a))
	ty, err := ctx.Infer(Var("r"), env)
	if err != nil {
		t.Fatal(err)
	}
	typeString := types.TypeString(ty)
	if typeString != "weak '_0 => ref['_0]" {
		t.Fatalf("types within references should not be generalized: %s", typeString)
	}

	a = env.NewGenericVar()
	env.Declare("new", TArrow(nil, TRef(a)))
	expr := RecordExtend(RecordEmpty(), LabelValue("id", Func1("x", Var("x"))), LabelValue("r", Call(Var("new"))))
	ty, err = ctx.Infer(expr, env)
	if err != nil {
		t.Fatal(err)
	}
	typeString = types.TypeString(ty)
	if typeString != "weak '_3 => {id : 'a -> 'a, r : ref['_3]}" {
		t.Fatalf("types within references should not be generalized: %s", typeString)
	}

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

	var expr ast.Expr = RecordSelect(record, "a")

	ty, err := ctx.Infer(expr, env)
	if err != nil {
		t.Fatal(err)
	}
	typeString := types.TypeString(ty)
	if typeString != "A" {
		t.Fatalf("expected A, found %s", typeString)
	}

	expr = RecordRestrict(record, "a")

	ty, err = ctx.Infer(expr, env)
	if err != nil {
		t.Fatal(err)
	}
	typeString = types.TypeString(ty)
	if typeString != "{b : B}" {
		t.Fatalf("expected {b : B}, found %s", typeString)
	}
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

	ty, err := ctx.Infer(Var("someints"), env)
	if err != nil {
		t.Fatal(err)
	}
	if types.TypeString(ty) != "{cap : int, data : ref[array[int]], len : int}" {
		t.Fatalf("type: %s", types.TypeString(ty))
	}

	ty, err = ctx.Infer(Var("someints2"), env)
	if err != nil {
		t.Fatal(err)
	}
	if types.TypeString(ty) != "slice[int]" {
		t.Fatalf("type: %s", types.TypeString(ty))
	}

	sel := Let("f", Func1("x", RecordSelect(Var("x"), "data")),
		Call(Var("f"), Var("someints2")))

	ty, err = ctx.Infer(sel, env)
	if err != nil {
		t.Fatal(err)
	}
	if types.TypeString(ty) != "ref[array[int]]" {
		t.Fatalf("type: %s", types.TypeString(ty))
	}

	ty, err = ctx.Infer(Call(Var("append"), Var("someints"), Var("someints")), env)
	if err != nil {
		t.Fatal(err)
	}
	if types.TypeString(ty) != "slice[int]" {
		t.Fatalf("type: %s", types.TypeString(ty))
	}

	ty, err = ctx.Infer(Call(Var("append"), Var("someints"), Var("someints2")), env)
	if err != nil {
		t.Fatal(err)
	}
	if types.TypeString(ty) != "slice[int]" {
		t.Fatalf("type: %s", types.TypeString(ty))
	}

	ty, err = ctx.Infer(Call(Var("append"), Var("someints2"), Var("someints3")), env)
	if err != nil {
		t.Fatal(err)
	}
	if types.TypeString(ty) != "slice[int]" {
		t.Fatalf("type: %s", types.TypeString(ty))
	}
}

func TestMutuallyRecursiveTypes(t *testing.T) {
	env := NewTypeEnv(nil)
	ctx := NewContext()

	cycle, err := env.NewRecursive(func(rec *types.Recursive) {
		int2bool := TApp(TConst("cycle"), TConst("int"), TConst("bool"))
		bool2int := TApp(TConst("cycle"), TConst("bool"), TConst("int"))
		rec.AddType("int2bool", int2bool)
		rec.AddType("bool2int", bool2int)
		int2bool.Underlying = TRecord(TRowExtend(types.RowEmptyPointer, TypeMap(map[string]types.Type{
			"v":    TConst("int"),
			"link": &types.RecursiveLink{Recursive: rec, Index: rec.Indexes["bool2int"]},
		})))
		bool2int.Underlying = TRecord(TRowExtend(types.RowEmptyPointer, TypeMap(map[string]types.Type{
			"v":    TConst("bool"),
			"link": &types.RecursiveLink{Recursive: rec, Index: rec.Indexes["int2bool"]},
		})))
	})
	if err != nil {
		t.Fatal(err)
	}

	int2bool := cycle.GetType("int2bool")
	env.Declare("someint2bool", int2bool)

	var expr ast.Expr = Var("someint2bool")

	ty, err := ctx.Infer(expr, env)
	if err != nil {
		t.Fatal(err)
	}
	if types.TypeString(ty) != "cycle[int, bool]" {
		t.Fatalf("type: %s", types.TypeString(ty))
	}

	expr = Let("f", Func1("x", RecordSelect(Var("x"), "link")),
		Call(Var("f"), Var("someint2bool")))

	ty, err = ctx.Infer(expr, env)
	if err != nil {
		t.Fatal(err)
	}
	if types.TypeString(ty) != "cycle[bool, int]" {
		t.Fatalf("type: %s", types.TypeString(ty))
	}

	expr = Let("f", Func1("x", RecordSelect(Var("x"), "v")),
		Call(Var("f"), Var("someint2bool")))

	ty, err = ctx.Infer(expr, env)
	if err != nil {
		t.Fatal(err)
	}
	if types.TypeString(ty) != "int" {
		t.Fatalf("type: %s", types.TypeString(ty))
	}

	expr = Let("link", RecordSelect(Var("someint2bool"), "link"),
		RecordSelect(Var("link"), "link"))

	ty, err = ctx.Infer(expr, env)
	if err != nil {
		t.Fatal(err)
	}
	if types.TypeString(ty) != "cycle[int, bool]" {
		t.Fatalf("type: %s", types.TypeString(ty))
	}

	expr = RecordSelect(RecordSelect(RecordSelect(RecordSelect(Var("someint2bool"), "link"), "link"), "link"), "v")

	ty, err = ctx.Infer(expr, env)
	if err != nil {
		t.Fatal(err)
	}
	if types.TypeString(ty) != "bool" {
		t.Fatalf("type: %s", types.TypeString(ty))
	}
}

func TestGenericMutuallyRecursiveTypes(t *testing.T) {
	env := NewTypeEnv(nil)
	ctx := NewContext()

	cycle, err := env.NewRecursive(func(rec *types.Recursive) {
		a, b := env.NewGenericVar(), env.NewGenericVar()
		a2b := TApp(TConst("cycle"), a, b)
		b2a := TApp(TConst("cycle"), b, a)
		rec.AddType("a2b", a2b)
		rec.AddType("b2a", b2a)
		a2b.Underlying = TRecord(TRowExtend(types.RowEmptyPointer, TypeMap(map[string]types.Type{
			"v":    a,
			"link": &types.RecursiveLink{Recursive: rec, Index: rec.Indexes["b2a"]},
		})))
		b2a.Underlying = TRecord(TRowExtend(types.RowEmptyPointer, TypeMap(map[string]types.Type{
			"v":    b,
			"link": &types.RecursiveLink{Recursive: rec, Index: rec.Indexes["a2b"]},
		})))
	})
	if err != nil {
		t.Fatal(err)
	}

	a2b := cycle.GetType("a2b")
	a2b = types.Generalize(a2b).(*types.App)
	env.Declare("a2bid", TArrow1(a2b, a2b))

	intboolcycle, err := env.NewRecursiveInstance(cycle, func(rec *types.Recursive) {
		int2bool := TApp(TConst("cycle"), TConst("int"), TConst("bool"))
		bool2int := TApp(TConst("cycle"), TConst("bool"), TConst("int"))
		rec.AddType("int2bool", int2bool)
		rec.AddType("bool2int", bool2int)
		int2bool.Underlying = TRecord(TRowExtend(types.RowEmptyPointer, TypeMap(map[string]types.Type{
			"v":    TConst("int"),
			"link": &types.RecursiveLink{Recursive: rec, Index: rec.Indexes["bool2int"]},
		})))
		bool2int.Underlying = TRecord(TRowExtend(types.RowEmptyPointer, TypeMap(map[string]types.Type{
			"v":    TConst("bool"),
			"link": &types.RecursiveLink{Recursive: rec, Index: rec.Indexes["int2bool"]},
		})))
	})
	if err != nil {
		t.Fatal(err)
	}

	int2bool := intboolcycle.GetType("int2bool")
	env.Declare("someint2bool", int2bool)

	expr := Let("x", Call(Var("a2bid"), Var("someint2bool")),
		Let("y", RecordSelect(Var("x"), "link"),
			Let("z", RecordSelect(Var("y"), "link"),
				Var("z"))))

	ty, err := ctx.Infer(expr, env)
	if err != nil {
		t.Fatal(err)
	}
	if types.TypeString(ty) != "cycle[int, bool]" {
		t.Fatalf("type: %s", types.TypeString(ty))
	}

	// invalid:

	_, err = env.NewRecursiveInstance(cycle, func(rec *types.Recursive) {
		int2bool := TApp(TConst("cycle"), TConst("int"), TConst("bool"))
		bool2int := TApp(TConst("cycle"), TConst("bool"), TConst("int"))
		rec.AddType("int2bool", int2bool)
		rec.AddType("bool2int", bool2int)
		int2bool.Underlying = TRecord(TRowExtend(types.RowEmptyPointer, TypeMap(map[string]types.Type{
			"v":    TConst("int"),
			"link": &types.RecursiveLink{Recursive: rec, Index: rec.Indexes["bool2int"]},
		})))
		bool2int.Underlying = TRecord(TRowExtend(types.RowEmptyPointer, TypeMap(map[string]types.Type{
			"v":    TConst("bool"),
			"link": &types.RecursiveLink{Recursive: rec, Index: rec.Indexes["bool2int"]}, // <-- should not unify
		})))
	})
	if err == nil {
		t.Fatalf("expected unification error for invalid recursive instance")
	}
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
	ty, err := ctx.Infer(expr, env)
	if err != nil {
		t.Fatal(err)
	}
	if types.TypeString(ty) != "int -> int" {
		t.Fatalf("type: %s", types.TypeString(ty))
	}
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
	ty, err := ctx.Infer(expr, env)
	if err != nil {
		t.Fatal(err)
	}
	if types.TypeString(ty) != "{f : int -> int, g : int -> int, h : int -> int, id : 'a -> 'a}" {
		t.Fatalf("type: %s", types.TypeString(ty))
	}
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

	ty, err := ctx.Infer(ifExpr, env)
	if err != nil {
		t.Fatal(err)
	}
	if types.TypeString(ty) != "[b : bool, i : int | 'a]" {
		t.Fatalf("type: %s", types.TypeString(ty))
	}

	matchExpr := Match(ifExpr, // [i : int, b : bool | 'a]
		[]ast.MatchCase{
			{Label: "i", Var: "i", Value: Call(Var("add"), Var("i"), Var("i"))},
			{Label: "b", Var: "b", Value: Call(Var("if"), Var("b"), Var("someint"), Var("someint"))},
		},
		// default case:
		nil)

	ty, err = ctx.Infer(matchExpr, env)
	if err != nil {
		t.Fatal(err)
	}
	if types.TypeString(ty) != "int" {
		t.Fatalf("type: %s", types.TypeString(ty))
	}

	matchExpr = Match(ifExpr, // [i : int, b : bool | 'a]
		[]ast.MatchCase{
			{Label: "i", Var: "i", Value: Call(Var("add"), Var("i"), Var("i"))},
			// does not match variant type for the match argument (bool != int):
			{Label: "b", Var: "b", Value: Call(Var("add"), Var("b"), Var("b"))},
		},
		// default case:
		nil)

	ty, err = ctx.Infer(matchExpr, env)
	if err == nil {
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
	ty, err = ctx.Infer(expr, env)
	if err != nil {
		t.Fatal(err)
	}
	if types.TypeString(ty) != "[a : int, b : int, c : int | 'a] -> int" {
		t.Fatalf("type: %s", types.TypeString(ty))
	}

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
	ty, err = ctx.Infer(fnExpr, env)
	if err != nil {
		t.Fatal(err)
	}
	if types.TypeString(ty) != "([a : int, b : int, c : 'a], int) -> int" {
		t.Fatalf("type: %s", types.TypeString(ty))
	}

	// Call:

	callExpr := Call(fnExpr, Variant("c", Var("somebool")), Var("someint"))

	if ast.ExprString(callExpr) != "(fn (x, y) -> match x { :a i -> add(i, i) | :b i -> add(i, i) | :c _ -> y })(:c somebool, someint)" {
		t.Fatalf("expr: %s", ast.ExprString(callExpr))
	}
	ty, err = ctx.Infer(callExpr, env)
	if err != nil {
		t.Fatal(err)
	}
	if types.TypeString(ty) != "int" {
		t.Fatalf("type: %s", types.TypeString(ty))
	}
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

	ty, err := ctx.Infer(expr, env)
	if err != nil {
		t.Fatal(err)
	}
	if types.TypeString(ty) != "bool" {
		t.Fatalf("type: %s", types.TypeString(ty))
	}

	expr = Let("stack", RecordEmpty(),
		Let("stack", Call(Var("push"), Var("stack"), Var("i")), // i
			Let("stack", Call(Var("push"), Var("stack"), Var("b")), // i b
				Let("stack", Call(Var("pop"), Var("stack")), // i
					Let("top", Call(Var("peek"), Var("stack")), // i
						Let("stack", Call(Var("pop"), Var("stack")), // empty
							Var("top")))))))

	ty, err = ctx.Infer(expr, env)
	if err != nil {
		t.Fatal(err)
	}
	if types.TypeString(ty) != "int" {
		t.Fatalf("type: %s", types.TypeString(ty))
	}

	expr = Let("stack", RecordEmpty(),
		Let("stack", Call(Var("push"), Var("stack"), Var("i")), // i
			Let("stack", Call(Var("push"), Var("stack"), Var("b")), // i b
				Let("stack", Call(Var("pop"), Var("stack")), // i
					Let("stack", Call(Var("pop"), Var("stack")), // empty
						Let("stack", Call(Var("pop"), Var("stack")), // underflow
							Var("stack")))))))

	ty, err = ctx.Infer(expr, env)
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
	ty, err := ctx.Infer(expr, env)
	if err != nil {
		t.Fatal(err)
	}
	typeString := types.TypeString(ty)
	if typeString != "Option[string]" {
		t.Fatalf("type: %s", typeString)
	}

	expr = Call(Var("(>>=)"), Var("someintoption"), Func1("i", Call(Var("pure"), Call(Var("itoa"), Var("i")))))
	if ast.ExprString(expr) != "(>>=)(someintoption, fn (i) -> pure(itoa(i)))" {
		t.Fatalf("expr: %s", ast.ExprString(expr))
	}
	ty, err = ctx.Infer(expr, env)
	if err != nil {
		t.Fatal(err)
	}
	typeString = types.TypeString(ty)
	if typeString != "Option[string]" {
		t.Fatalf("type: %s", typeString)
	}

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
	ty, err = ctx.Infer(expr, env)
	if err != nil {
		t.Fatal(err)
	}
	typeString = types.TypeString(ty)
	if typeString != "ref[string]" {
		t.Fatalf("type: %s", typeString)
	}

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
	ty, err := ctx.Infer(call, env)
	if err != nil {
		t.Fatal(err)
	}
	typeString := types.TypeString(ty)
	if typeString != "A" {
		t.Fatalf("type: %s", typeString)
	}

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

	ty, err = ctx.Infer(addWrapper, env)
	if err != nil {
		t.Fatal(err)
	}
	typeString = types.TypeString(ty)
	if typeString != "Add 'a => ('a, 'a) -> 'a" {
		t.Fatalf("type: %s", typeString)
	}

	var expr ast.Expr = Call(addWrapper, Var("someint"), Var("someint"))
	ty, err = ctx.Infer(expr, env)
	if err != nil {
		t.Fatal(err)
	}
	if tt, ok := ty.(*types.Const); !ok || tt.Name != "int" {
		t.Fatalf("expected int return, found " + tt.Name)
	}

	expr = Call(addWrapper, Var("someshort"), Var("someshort"))
	ty, err = ctx.Infer(expr, env)
	if err != nil {
		t.Fatal(err)
	}
	if tt, ok := ty.(*types.Const); !ok || tt.Name != "short" {
		t.Fatalf("expected short return, found " + tt.Name)
	}

	expr = Call(addWrapper, Var("somefloat"), Var("somefloat"))
	ty, err = ctx.Infer(expr, env)
	if err != nil {
		t.Fatal(err)
	}
	if tt, ok := ty.(*types.Const); !ok || tt.Name != "float" {
		t.Fatalf("expected float return, found " + tt.Name)
	}

	expr = Call(addWrapper, Var("someint"), Var("somefloat"))
	ty, err = ctx.Infer(expr, env)
	if err == nil {
		t.Fatalf("expected constraint error")
	}

	expr = Call(addWrapper, Var("somebool"), Var("somebool"))
	ty, err = ctx.Infer(expr, env)
	if err == nil {
		t.Fatalf("expected constraint error")
	}

	expr = Call(addWrapper, Var("someintvec"), Var("someintvec"))
	ty, err = ctx.Infer(expr, env)
	if err != nil {
		t.Fatal(err)
	}
	typeString = types.TypeString(ty)
	if typeString != "vec[int]" {
		t.Fatalf("type: %s", typeString)
	}

	expr = Call(addWrapper, Var("somefloatvec"), Var("somefloatvec"))
	ty, err = ctx.Infer(expr, env)
	if err != nil {
		t.Fatal(err)
	}
	typeString = types.TypeString(ty)
	if typeString != "vec[float]" {
		t.Fatalf("type: %s", typeString)
	}

	expr = Call(addWrapper, Var("someboolvec"), Var("someboolvec"))
	ty, err = ctx.Infer(expr, env)
	if err == nil {
		t.Fatalf("expected constraint error")
	}

	expr = Call(Var("&"), Var("someint"), Var("someint"))
	ty, err = ctx.Infer(expr, env)
	if err != nil {
		t.Fatal(err)
	}
	if tt, ok := ty.(*types.Const); !ok || tt.Name != "int" {
		t.Fatalf("expected int return, found " + tt.Name)
	}

	expr = Call(Var("&"), Var("someshort"), Var("someshort"))
	ty, err = ctx.Infer(expr, env)
	if err != nil {
		t.Fatal(err)
	}
	if tt, ok := ty.(*types.Const); !ok || tt.Name != "short" {
		t.Fatalf("expected short return, found " + tt.Name)
	}

	expr = Call(Var("&"), Var("somebool"), Var("somebool"))
	_, err = ctx.Infer(expr, env)
	if err == nil {
		t.Fatalf("expected constraint error")
	}

	expr = Call(Var("&"), Var("someint"), Var("someshort"))
	_, err = ctx.Infer(expr, env)
	if err == nil {
		t.Fatalf("expected constraint error")
	}

	addWrapper = Func1("x", Call(Var("+"), Var("x"), Var("someint")))
	ty, err = ctx.Infer(addWrapper, env)
	if err != nil {
		t.Fatal(err)
	}
	typeString = types.TypeString(ty)
	if typeString != "int -> int" {
		t.Fatalf("expected int -> int, found %s", typeString)
	}

	// instance validation

	if _, err := env.DeclareInstance(Int, boolType, map[string]string{"&": "bool_and"}); err == nil {
		t.Fatalf("expected missing-method error")
	}
	if _, err := env.DeclareInstance(Add, boolType, map[string]string{"+": "bad_bool_add"}); err == nil {
		t.Fatalf("expected invalid-method error")
	}
}

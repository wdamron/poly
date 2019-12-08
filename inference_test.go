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
	"reflect"
	"strings"
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

func TestUnit(t *testing.T) {
	ty := TArrow1(TConst("int"), TUnit())
	if types.TypeString(ty) != "int -> ()" {
		t.Fatalf("type: %s", types.TypeString(ty))
	}
}

func TestLiterals(t *testing.T) {
	env := NewTypeEnv(nil)
	ctx := NewContext()

	env.Declare("x", TConst("int"))

	xvec := &ast.Literal{
		Syntax: "[x]", Using: []string{"x"},
		Construct: func(env types.TypeEnv, level uint, using []types.Type) (types.Type, error) {
			return TApp(TConst("vec"), using[0]), nil // x :: int |- vec[int]
		},
	}
	mustInfer(t, env, ctx, Let("vec", xvec, Var("vec")), "vec[int]")
}

func TestSizes(t *testing.T) {
	env := NewTypeEnv(nil)
	ctx := NewContext()

	intarray := TApp(TConst("array"), TConst("int"), env.NewGenericSize())

	env.Declare("xs", TApp(TConst("array"), TConst("int"), TSize(8)))
	env.Declare("ys", TApp(TConst("array"), TConst("int"), TSize(16)))
	env.Declare("zs", TApp(TConst("array"), TConst("int"), TConst("foo")))
	env.Declare("add", TArrow2(intarray, intarray, intarray))

	sharedSize := env.NewGenericSize()
	env.Declare("get", TArrow2(
		TApp(TConst("array"), TConst("int"), sharedSize),
		TApp(TConst("index"), sharedSize),
		TConst("int")))

	env.Declare("xi", TApp(TConst("index"), TSize(8)))
	env.Declare("yi", TApp(TConst("index"), TSize(16)))

	mustInfer(t, env, ctx, Var("get"), "size 'a => (array[int, 'a], index['a]) -> int")
	mustInfer(t, env, ctx, Call(Var("get"), Var("xs"), Var("xi")), "int")
	mustInfer(t, env, ctx, Call(Var("get"), Var("ys"), Var("yi")), "int")
	if _, err := ctx.Infer(Call(Var("get"), Var("xs"), Var("yi")), env); err == nil {
		t.Fatalf("Expected size-mismatch error")
	}

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
	slice := Generalize(TAlias(TApp(TConst("slice"), a), sliceHeader))

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

func TestVariableScopeAnnotations(t *testing.T) {
	env := NewTypeEnv(nil)
	ctx := NewContext()

	expr := Func2("x", "y",
		LetGroup([]ast.LetBinding{
			{"a", Var("x")},
			{"b", Var("y")},
		},
			Let("z", Var("a"),
				Let("z2", Var("b"),
					Let("z2", Var("b"),
						Var("z2"))))))

	if err := ctx.AnnotateDirect(expr, env); err != nil {
		t.Fatal(err)
	}

	var seen []string
	ast.WalkExpr(expr, func(e ast.Expr) {
		if v, ok := e.(*ast.Var); ok {
			if v.Scope() == nil {
				t.Fatalf("nil scope annotation for %s", v.Name)
			}
			seen = append(seen, v.Name)
			switch v.Name {
			case "x", "y":
				if _, ok := v.Scope().Expr.(*ast.Func); !ok {
					t.Fatalf("wrong scope annotation for %s, found %s", v.Name, v.Scope().Expr.ExprName())
				}

			case "a", "b":
				if _, ok := v.Scope().Expr.(*ast.LetGroup); !ok {
					t.Fatalf("wrong scope annotation for %s, found %s", v.Name, v.Scope().Expr.ExprName())
				}

			case "z2":
				if _, ok := v.Scope().Expr.(*ast.Let); !ok {
					t.Fatalf("wrong scope annotation for %s, found %s", v.Name, v.Scope().Expr.ExprName())
				}
				if v.Scope().Parent == nil {
					t.Logf("nil parent annotation for %s", v.Name)
				}
				if _, ok := v.Scope().Parent.Parent.Parent.Expr.(*ast.LetGroup); !ok {
					t.Fatalf("wrong parent annotation for z2, found %s", v.Scope().Parent.Expr.ExprName())
				}
				if _, ok := v.Scope().Parent.Parent.Parent.Parent.Expr.(*ast.Func); !ok {
					t.Fatalf("wrong scope annotation for top parent of z2, found %s", v.Scope().Expr.ExprName())
				}
			}
		}
	})

	for i, name := range []string{"x", "y", "a", "b", "b", "z2"} {
		if seen[i] != name {
			t.Fatalf("not all variables were visited/checked: %#+v", seen)
		}
	}

	env.Declare("x", TConst("int"))
	varExpr := Var("x")
	if err := ctx.AnnotateDirect(varExpr, env); err != nil {
		t.Fatal(err)
	}
	if varExpr.Scope() != ast.PredeclaredScope {
		t.Fatalf("expected predeclared scope")
	}
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

func TestControlFlow(t *testing.T) {
	env := NewTypeEnv(nil)
	ctx := NewContext()

	env.Declare("n", TConst("int"))
	env.Declare("b", TConst("bool"))
	env.Declare("zero", TConst("int"))
	env.Declare("dec", TArrow1(TConst("int"), TConst("int")))
	env.Declare("cmp", TArrow2(TConst("int"), TConst("int"), TConst("bool")))

	cfg := ControlFlow("strange_loop", "local_x")

	cfg.SetEntry(
		DerefAssign(Var("local_x"), Call(Var("dec"), Var("n"))),
		Call(Var("cmp"), Deref(Var("local_x")), Var("zero")))

	cfg.SetReturn(Deref(Var("local_x")))

	L0 := cfg.AddBlock(
		DerefAssign(Var("local_x"), Call(Var("dec"), Deref(Var("local_x")))),
		Call(Var("cmp"), Deref(Var("local_x")), Var("zero")))

	L1 := cfg.AddBlock(
		DerefAssign(Var("local_x"), Call(Var("dec"), Deref(Var("local_x")))),
		Call(Var("cmp"), Deref(Var("local_x")), Var("zero")))

	cfg.AddJump(cfg.Entry, cfg.Return)
	cfg.AddJump(cfg.Entry, L0)
	cfg.AddJump(L0, L1)
	cfg.AddJump(L1, L0)
	cfg.AddJump(L1, cfg.Return)

	mustInfer(t, env, ctx, cfg, "int")

	expect := "strange_loop(local_x) {" +
		"entry : {*local_x = dec(n); cmp(*local_x, zero)}, " +
		"return : *local_x, " +
		"L0 : {*local_x = dec(*local_x); cmp(*local_x, zero)}, " +
		"L1 : {*local_x = dec(*local_x); cmp(*local_x, zero)}" +
		"} in {entry -> [return, L0], L0 -> [L1], L1 -> [return, L0]}"

	if ast.ExprString(cfg) != expect {
		t.Fatalf("expr: %s", ast.ExprString(cfg))
	}

	// Error detection for loops (inferred as recursive functions):

	cfg = ControlFlow("invalid_loop", "local_n", "local_b", "local_row")

	cfg.SetEntry(
		DerefAssign(Var("local_n"), Var("n")),
		DerefAssign(Var("local_b"), Var("b")),
		DerefAssign(Var("local_row"), RecordExtend(RecordEmpty(),
			LabelValue("n", Var("n")),
			LabelValue("b", Var("b")))),
		Call(Var("cmp"), Deref(Var("local_n")), Var("zero")))

	cfg.SetReturn(Deref(Var("local_row")))

	// Record extension within loops is not supported (leads to a recursive type error):

	L0 = cfg.AddBlock(
		DerefAssign(Var("local_row"), RecordExtend(Deref(Var("local_row")),
			LabelValue("n", Deref(Var("local_n"))),
			LabelValue("b", Deref(Var("local_b"))))),
		DerefAssign(Var("local_n"), Call(Var("dec"), Deref(Var("local_n")))),
		Call(Var("cmp"), Deref(Var("local_n")), Var("zero")))

	L1 = cfg.AddBlock(
		DerefAssign(Var("local_row"), RecordExtend(Deref(Var("local_row")),
			LabelValue("n", Deref(Var("local_n"))),
			LabelValue("b", Deref(Var("local_b"))))),
		DerefAssign(Var("local_n"), Call(Var("dec"), Deref(Var("local_n")))),
		Call(Var("cmp"), Deref(Var("local_n")), Var("zero")))

	cfg.AddJump(cfg.Entry, cfg.Return)
	cfg.AddJump(cfg.Entry, L0)
	cfg.AddJump(L0, L1)
	cfg.AddJump(L1, L0)
	cfg.AddJump(L1, cfg.Return)

	if _, err := ctx.Infer(cfg, env); err == nil {
		t.Fatalf("expected recursive type error")
	}
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

	var expr ast.Expr
	expr =
		Func1("x",
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

	// Record extension within recursive functions is not supported:

	env.Declare("someint", TConst("int"))

	expr =
		Let("f", Func1("x", RecordExtend(Call(Var("f"), Var("x")), LabelValue("i", Var("someint")))),
			Var("f"))

	if _, err := ctx.Infer(expr, env); err == nil {
		t.Fatalf("expected recursive type error")
	}
}

func TestFixedPoint(t *testing.T) {
	env := NewTypeEnv(nil)
	ctx := NewContext()

	// fix :: ('a -> 'a) -> 'a
	// fix = fn (f) -> f(fix(f))
	a := env.NewGenericVar()
	env.Declare("fix", TArrow1(TArrow1(a, a), a))

	a = env.NewGenericVar()
	env.Declare("merge", TArrow2(a, a, a))

	// fix(fn (f) -> fn (x) -> merge(x, f(x)))
	expr := Call(Var("fix"), Func1("f", Func1("x", Call(Var("merge"), Var("x"), Call(Var("f"), Var("x"))))))

	mustInfer(t, env, ctx, expr, "'a -> 'a")

	a = env.NewGenericVar()
	env.Declare("if", TArrow3(TConst("bool"), a, a, a))
	env.Declare("sub", TArrow2(TConst("int"), TConst("int"), TConst("int")))
	env.Declare("mul", TArrow2(TConst("int"), TConst("int"), TConst("int")))
	env.Declare("less", TArrow2(TConst("int"), TConst("int"), TConst("bool")))
	env.Declare("1", TConst("int"))

	// fix(fn (fact) -> fn (n) -> if(less(n, 1), 1, mul(n, fact(sub(n, 1)))))
	expr =
		Call(Var("fix"), Func1("fact", Func1("n",
			Call(Var("if"), Call(Var("less"), Var("n"), Var("1")),
				Var("1"),
				Call(Var("mul"), Var("n"), Call(Var("fact"), Call(Var("sub"), Var("n"), Var("1"))))))))

	mustInfer(t, env, ctx, expr, "int -> int")

	// Record extension within recursive functions is not supported:
	// fix(fn (f) -> fn (x) -> {i = 1 | f(x)})
	expr = Call(Var("fix"), Func1("f", Func1("x", RecordExtend(Call(Var("f"), Var("x")), LabelValue("i", Var("1"))))))

	if _, err := ctx.Infer(expr, env); err == nil {
		t.Fatalf("expected recursive type error")
	}
}

func TestMutuallyRecursiveLet(t *testing.T) {
	env := NewTypeEnv(nil)
	ctx := NewContext()

	env.Declare("add", TArrow2(TConst("int"), TConst("int"), TConst("int")))
	A := env.NewGenericVar()
	env.Declare("if", TArrow3(TConst("bool"), A, A, A))
	env.Declare("somebool", TConst("bool"))

	somebool, add, f, g, h, id, x := Var("somebool"), Var("add"), Var("f"), Var("g"), Var("h"), Var("id"), Var("x")

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

	if err := ctx.AnnotateDirect(expr, env); err != nil {
		t.Fatal(err)
	}
	sccs := expr.StronglyConnectedComponents()
	sccNames := make([][]string, len(sccs))
	for i, scc := range sccs {
		sccNames[i] = make([]string, len(scc))
		for j, binding := range scc {
			sccNames[i][j] = binding.Var
		}
	}
	if !reflect.DeepEqual([][]string{{"id"}, {"g", "f"}}, sccNames) {
		t.Fatalf("invalid strongly connected components, found %#+v", sccNames)
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

	// push(i), push(b), peek, pop, pop -> b
	expr := Let("stack", RecordEmpty(),
		Let("stack", Call(Var("push"), Var("stack"), Var("i")), // i
			Let("stack", Call(Var("push"), Var("stack"), Var("b")), // i b
				Let("top", Call(Var("peek"), Var("stack")), // i b
					Let("stack", Call(Var("pop"), Var("stack")), // i,
						Let("stack", Call(Var("pop"), Var("stack")), // empty
							Var("top")))))))

	mustInfer(t, env, ctx, expr, "bool")

	// push(i), push(b), pop, peek, pop -> i
	expr = Let("stack", RecordEmpty(),
		Let("stack", Call(Var("push"), Var("stack"), Var("i")), // i
			Let("stack", Call(Var("push"), Var("stack"), Var("b")), // i b
				Let("stack", Call(Var("pop"), Var("stack")), // i
					Let("top", Call(Var("peek"), Var("stack")), // i
						Let("stack", Call(Var("pop"), Var("stack")), // empty
							Var("top")))))))

	mustInfer(t, env, ctx, expr, "int")

	// push(i), push(b), pop, pop, pop -> underflow
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
		f.RestrictConstVar()
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
		f.RestrictConstVar()
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
		m.RestrictConstVar()
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
	env.Declare("atoi", TArrow1(TConst("string"), TConst("int")))
	env.Declare("someintoption", TApp(option, TConst("int")))

	var expr ast.Expr = Call(Var("fmap"), Var("itoa"), Var("someintoption"))
	mustInfer(t, env, ctx, expr, "Option[string]")

	expr = Call(Var("(>>=)"), Var("someintoption"), Func1("i", Call(Var("pure"), Call(Var("itoa"), Var("i")))))
	if ast.ExprString(expr) != "(>>=)(someintoption, fn (i) -> pure(itoa(i)))" {
		t.Fatalf("expr: %s", ast.ExprString(expr))
	}
	mustInfer(t, env, ctx, expr, "Option[string]")

	expr = Func1("x", Pipe("$", Var("x"),
		Call(Var("(>>=)"), Var("$"), Func1("i", Call(Var("pure"), Call(Var("itoa"), Var("i"))))),
		Call(Var("(>>=)"), Var("$"), Func1("s", Call(Var("pure"), Call(Var("atoi"), Var("s"))))),
		Call(Var("(>>=)"), Var("$"), Func1("i", Call(Var("some"), Var("i"))))))

	mustInfer(t, env, ctx, expr, "Option[int] -> Option[int]")

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

func TestUnionTypeClasses(t *testing.T) {
	env := NewTypeEnv(nil)
	ctx := NewContext()

	ABC, err := env.DeclareUnionTypeClass("ABC", nil, map[string]types.Type{
		"A": TConst("A"),
		"B": TConst("B"),
		"C": TConst("C"),
	})
	if err != nil {
		t.Fatal(err)
	}
	abc := env.NewQualifiedVar(types.InstanceConstraint{ABC})
	env.Declare("fabc", TArrow1(abc, abc))
	env.Declare("somea", TConst("A"))
	env.Declare("someint", TConst("int"))

	call := Call(Var("fabc"), Var("somea"))
	mustInfer(t, env, ctx, call, "A")

	// (built-in) cast to a tagged variant:
	mustInfer(t, env, ctx, Var("ABC"), "ABC 'a => 'a -> [A : A, B : B, C : C]")

	call = Call(Var("ABC"), Var("somea"))
	mustInfer(t, env, ctx, call, "[A : A, B : B, C : C]")

	match := Match(Call(Var("ABC"), Var("somea")),
		[]ast.MatchCase{
			{Label: "A", Var: "a", Value: Var("someint")},
			{Label: "B", Var: "b", Value: Var("someint")},
			{Label: "C", Var: "c", Value: Var("someint")},
		},
		// no default case:
		nil)

	mustInfer(t, env, ctx, match, "int")
}

func TestDeferredInstanceMatching(t *testing.T) {
	env := NewTypeEnv(nil)
	ctx := NewContext()

	objA := TRecordFlat(map[string]types.Type{"x": TConst("A")})
	objB := TRecordFlat(map[string]types.Type{"x": TConst("B")})

	HasX, err := env.DeclareUnionTypeClass("HasX", nil, map[string]types.Type{
		"A": objA,
		"B": objB,
	})
	if err != nil {
		t.Fatal(err)
	}

	env.Declare("objA", objA)
	env.Declare("objB", objB)
	env.Declare("a_id", TArrow1(TConst("A"), TConst("A")))

	hasX := env.NewQualifiedVar(types.InstanceConstraint{HasX})
	env.Declare("obj_id", TArrow1(hasX, hasX))

	mustInfer(t, env, ctx, Var("obj_id"), "HasX 'a => 'a -> 'a")
	mustInfer(t, env, ctx, Var("objA"), "{x : A}")
	mustInfer(t, env, ctx, Call(Var("obj_id"), Var("objA")), "{x : A}")

	// fn (x) -> let _ = obj_id({x = x}) in a_id(x)
	expr := Func1("x",
		Let("_", Call(Var("obj_id"), RecordExtend(RecordEmpty(), LabelValue("x", Var("x")))),
			Call(Var("a_id"), Var("x"))))

	if _, err = ctx.Infer(expr, env); err == nil {
		t.Fatalf("unexpected match for type-class with missing context")
	}
	if !strings.Contains(err.Error(), "Instance cannot be determined") {
		t.Fatalf("expected context error, found: %s", err.Error())
	}
	if ctx.InvalidExpr() == nil {
		t.Fatalf("expected invalid expression to be retained")
	}
	if ast.ExprString(ctx.InvalidExpr()) != "obj_id({x = x})" {
		t.Fatalf("invalid expr: %s", ast.ExprString(ctx.InvalidExpr()))
	}

	ctx.EnableDeferredInstanceMatching(true)
	if _, err = ctx.Infer(expr, env); err != nil {
		t.Fatal(err)
	}
}

func TestConstraints(t *testing.T) {
	env := NewTypeEnv(nil)
	ctx := NewContext()

	// type hierarchy

	intType, shortType, floatType, boolType := TConst("int"), TConst("short"), TConst("float"), TConst("bool")
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

	// method-instance lookups

	call := Call(Var("+"), Var("someint"), Var("someint"))
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

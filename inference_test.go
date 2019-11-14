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

func TestRecursiveLet(t *testing.T) {
	env := NewTypeEnv(nil)
	ctx := NewContext()

	env.Declare("add", TArrow2(TConst("int"), TConst("int"), TConst("int")))
	A := env.NewGenericVar()
	env.Declare("if", TArrow3(TConst("bool"), A, A, A))
	env.Declare("newbool", TArrow(nil, TConst("bool")))

	expr := Func1("x",
		Let("f",
			Func1("x",
				Call(
					Var("if"),
					Call(Var("newbool")),
					Var("x"),
					Call(Var("f"), Call(Var("add"), Var("x"), Var("x"))),
				),
			),
			Call(Var("f"), Var("x")),
		))

	exprString := ast.ExprString(expr)
	if exprString != "fn (x) -> let f(x) = if(newbool(), x, f(add(x, x))) in f(x)" {
		t.Fatalf("expr: %s", exprString)
	}

	// Infer twice to ensure state is properly reset between calls:

	envCount := len(env.Types)

	ty, err := ctx.Infer(expr, env)
	if err != nil {
		t.Fatal(err)
	}

	if len(env.Types) != envCount {
		t.Fatalf("expected unmodified type environment after inference")
	}

	ty, err = ctx.Infer(expr, env)
	if err != nil {
		t.Fatal(err)
	}

	typeString := types.TypeString(ty)
	if typeString != "int -> int" {
		t.Fatalf("type: %s", typeString)
	}
}

func TestRefs(t *testing.T) {
	env := NewTypeEnv(nil)
	ctx := NewContext()

	a := env.NewVar(-1)
	env.DeclareWeak("r", TRef(a))
	ty, err := ctx.Infer(Var("r"), env)
	if err != nil {
		t.Fatal(err)
	}
	typeString := types.TypeString(ty)
	if typeString != "ref['_0]" {
		t.Fatalf("types within references should not be generalized: %s", typeString)
	}

	a = env.NewGenericVar()
	env.Declare("new", TArrow(nil, TRef(a)))
	expr := RecordExtend(nil, LabelValue("id", Func1("x", Var("x"))), LabelValue("r", Call(Var("new"))))
	ty, err = ctx.Infer(expr, env)
	if err != nil {
		t.Fatal(err)
	}
	typeString = types.TypeString(ty)
	if typeString != "{id : 'a -> 'a, r : ref['_3]}" {
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

func TestVariantMatch(t *testing.T) {
	env := NewTypeEnv(nil)
	ctx := NewContext()

	env.Declare("add", TArrow2(TConst("int"), TConst("int"), TConst("int")))
	env.Declare("newint", TArrow(nil, TConst("int")))
	env.Declare("newbool", TArrow(nil, TConst("bool")))

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

	exprString := ast.ExprString(fnExpr)
	if exprString != "fn (x, y) -> match x { :a i -> add(i, i) | :b i -> add(i, i) | :c _ -> y }" {
		t.Fatalf("expr: %s", exprString)
	}

	ty, err := ctx.Infer(fnExpr, env)
	if err != nil {
		t.Fatal(err)
	}

	typeString := types.TypeString(ty)
	if typeString != "([a : int, b : int, c : 'a], int) -> int" {
		t.Fatalf("type: %s", typeString)
	}

	// Call:

	newint := Call(Var("newint"))
	newbool := Call(Var("newbool"))

	callExpr := Call(fnExpr, Variant("c", newbool), newint)

	exprString = ast.ExprString(callExpr)
	if exprString != "(fn (x, y) -> match x { :a i -> add(i, i) | :b i -> add(i, i) | :c _ -> y })(:c newbool(), newint())" {
		t.Fatalf("expr: %s", exprString)
	}

	ty, err = ctx.Infer(callExpr, env)
	if err != nil {
		t.Fatal(err)
	}

	typeString = types.TypeString(ty)
	if typeString != "int" {
		t.Fatalf("type: %s", typeString)
	}
}

func TestHigherKindedTypes(t *testing.T) {
	env := NewTypeEnv(nil)
	ctx := NewContext()

	// class Functor 'f where
	//   fmap :: (('a -> 'b), 'f['a]) -> 'f['b]
	Functor, err := env.DeclareTypeClass("Functor", func(f *types.Var) types.MethodSet {
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

func TestMutuallyRecursiveLet(t *testing.T) {
	env := NewTypeEnv(nil)
	ctx := NewContext()

	env.Declare("add", TArrow2(TConst("int"), TConst("int"), TConst("int")))
	A := env.NewGenericVar()
	env.Declare("if", TArrow3(TConst("bool"), A, A, A))
	env.Declare("newbool", TArrow(nil, TConst("bool")))

	newbool := Call(Var("newbool"))
	f := Var("f")
	g := Var("g")
	h := Var("h")
	id := Var("id")
	x := Var("x")

	expr := &ast.LetGroup{
		Vars: []ast.LetBinding{
			{
				Var:   "id",
				Value: Func1("x", x),
			},
			{
				Var: "f",
				Value: &ast.Func{
					ArgNames: []string{"x"},
					Body: &ast.Call{
						Func: Var("if"),
						Args: []ast.Expr{
							&ast.Call{
								Func: id,
								Args: []ast.Expr{newbool},
							},
							&ast.Call{
								Func: id,
								Args: []ast.Expr{x},
							},
							&ast.Call{
								Func: g,
								Args: []ast.Expr{&ast.Call{
									Func: Var("add"),
									Args: []ast.Expr{x, x},
								}},
							}},
					},
				},
			},
			{
				Var: "g",
				Value: &ast.Func{
					ArgNames: []string{"x"},
					Body: &ast.Call{
						Func: Var("if"),
						Args: []ast.Expr{
							newbool,
							x,
							&ast.Call{
								Func: id,
								Args: []ast.Expr{&ast.Call{
									Func: f,
									Args: []ast.Expr{x},
								}},
							},
						},
					},
				},
			},
		},
		Body: &ast.Let{
			Var: "h",
			Value: &ast.Func{
				ArgNames: []string{"x"},
				Body: &ast.Call{
					Func: id,
					Args: []ast.Expr{&ast.Call{
						Func: f,
						Args: []ast.Expr{x},
					}},
				},
			},
			Body: &ast.RecordExtend{
				Labels: []ast.LabelValue{
					{"f", f},
					{"g", g},
					{"h", h},
					{"id", id},
				},
			},
		},
	}

	exprString := ast.ExprString(expr)

	expect := "" +
		"let id(x) = x" +
		" and f(x) = if(id(newbool()), id(x), g(add(x, x)))" +
		" and g(x) = if(newbool(), x, id(f(x)))" +
		" in" +
		" let h(x) = id(f(x))" +
		" in {f = f, g = g, h = h, id = id}"

	if exprString != expect {
		t.Fatalf("expr: %s", exprString)
	}

	// Infer twice to ensure state is properly reset between calls:

	envCount := len(env.Types)

	ty, err := ctx.Infer(expr, env)
	if err != nil {
		t.Fatal(err)
	}

	if len(env.Types) != envCount {
		t.Fatalf("expected unmodified type environment after inference")
	}

	ty, err = ctx.Infer(expr, env)
	if err != nil {
		t.Fatal(err)
	}

	typeString := types.TypeString(ty)
	if typeString != "{f : int -> int, g : int -> int, h : int -> int, id : 'a -> 'a}" {
		t.Fatalf("type: %s", typeString)
	}
}

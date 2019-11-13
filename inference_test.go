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

package poly

import (
	"testing"

	"github.com/wdamron/poly/ast"
	"github.com/wdamron/poly/types"
)

func TestRecursiveLet(t *testing.T) {
	env := NewTypeEnv(nil)
	ctx := NewContext()

	env.Declare("add", &types.Arrow{
		Args:   []types.Type{&types.Const{"int"}, &types.Const{"int"}},
		Return: &types.Const{"int"},
	})
	A := env.NewGenericVar()
	env.Declare("if", &types.Arrow{
		Args:   []types.Type{&types.Const{"bool"}, A, A},
		Return: A,
	})
	env.Declare("newbool", &types.Arrow{Return: &types.Const{"bool"}})

	expr := &ast.Func{
		ArgNames: []string{"x"},
		Body: &ast.Let{
			Var: "f",
			Value: &ast.Func{
				ArgNames: []string{"x"},
				Body: &ast.Call{
					Func: &ast.Var{Name: "if"},
					Args: []ast.Expr{
						&ast.Call{Func: &ast.Var{Name: "newbool"}},
						&ast.Var{Name: "x"},
						&ast.Call{
							Func: &ast.Var{Name: "f"},
							Args: []ast.Expr{&ast.Call{
								Func: &ast.Var{Name: "add"},
								Args: []ast.Expr{&ast.Var{Name: "x"}, &ast.Var{Name: "x"}},
							}},
						}},
				},
			},
			Body: &ast.Call{
				Func: &ast.Var{Name: "f"},
				Args: []ast.Expr{&ast.Var{Name: "x"}},
			},
		},
	}

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
	env.DeclareWeak("r", types.NewRef(a))
	ty, err := ctx.Infer(&ast.Var{Name: "r"}, env)
	if err != nil {
		t.Fatal(err)
	}
	typeString := types.TypeString(ty)
	if typeString != "ref['_0]" {
		t.Fatalf("types within references should not be generalized: %s", typeString)
	}

	a = env.NewGenericVar()
	env.Declare("new", &types.Arrow{Return: types.NewRef(a)})
	expr := &ast.RecordExtend{
		Labels: []ast.LabelValue{
			{"id", &ast.Func{ArgNames: []string{"x"}, Body: &ast.Var{Name: "x"}}},
			{"r", &ast.Call{Func: &ast.Var{Name: "new"}}},
		},
	}
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

	env.Declare("a", &types.Const{"A"})
	env.Declare("b", &types.Const{"B"})

	record := &ast.RecordExtend{
		Labels: []ast.LabelValue{
			{"a", &ast.Var{Name: "a"}},
			{"b", &ast.Var{Name: "b"}},
		},
	}

	var expr ast.Expr = &ast.RecordSelect{
		Label:  "a",
		Record: record,
	}

	ty, err := ctx.Infer(expr, env)
	if err != nil {
		t.Fatal(err)
	}
	typeString := types.TypeString(ty)
	if typeString != "A" {
		t.Fatalf("expected A, found %s", typeString)
	}

	expr = &ast.RecordRestrict{
		Label:  "a",
		Record: record,
	}

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

	env.Declare("add", &types.Arrow{
		Args:   []types.Type{&types.Const{"int"}, &types.Const{"int"}},
		Return: &types.Const{"int"},
	})
	env.Declare("newint", &types.Arrow{Return: &types.Const{"int"}})
	env.Declare("newbool", &types.Arrow{Return: &types.Const{"bool"}})

	fnExpr := &ast.Func{
		ArgNames: []string{"x", "y"},
		Body: &ast.Match{
			Value: &ast.Var{Name: "x"},
			Cases: []ast.MatchCase{
				{Label: "a", Var: "i", Value: &ast.Call{
					Func: &ast.Var{Name: "add"},
					Args: []ast.Expr{&ast.Var{Name: "i"}, &ast.Var{Name: "i"}},
				}},
				{Label: "b", Var: "i", Value: &ast.Call{
					Func: &ast.Var{Name: "add"},
					Args: []ast.Expr{&ast.Var{Name: "i"}, &ast.Var{Name: "i"}},
				}},
				{Label: "c", Var: "_", Value: &ast.Var{Name: "y"}},
			},
		},
	}

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

	newint := &ast.Call{Func: &ast.Var{Name: "newint"}}
	newbool := &ast.Call{Func: &ast.Var{Name: "newbool"}}

	callExpr := &ast.Call{
		Func: fnExpr,
		Args: []ast.Expr{
			&ast.Variant{Label: "c", Value: newbool},
			newint,
		},
	}

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
			"fmap": &types.Arrow{
				Args: []types.Type{
					&types.Arrow{Args: []types.Type{a}, Return: b},
					&types.App{Const: f, Args: []types.Type{a}},
				},
				Return: &types.App{Const: f, Args: []types.Type{b}},
			},
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
			"pure": &types.Arrow{
				Args:   []types.Type{a},
				Return: &types.App{Const: f, Args: []types.Type{a}},
			},
			"(<*>)": &types.Arrow{
				Args: []types.Type{
					&types.App{Const: f, Args: []types.Type{&types.Arrow{Args: []types.Type{a}, Return: b}}},
					&types.App{Const: f, Args: []types.Type{a}},
				},
				Return: &types.App{Const: f, Args: []types.Type{b}},
			},
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
			"(>>=)": &types.Arrow{
				Args: []types.Type{
					&types.App{Const: m, Args: []types.Type{a}},
					&types.Arrow{Args: []types.Type{a}, Return: &types.App{Const: m, Args: []types.Type{b}}},
				},
				Return: &types.App{Const: m, Args: []types.Type{b}},
			},
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
	a, b, option := env.NewGenericVar(), env.NewGenericVar(), &types.Const{"Option"}
	env.Declare("option_map", &types.Arrow{
		Args: []types.Type{
			&types.Arrow{Args: []types.Type{a}, Return: b},
			&types.App{Const: option, Args: []types.Type{a}},
		},
		Return: &types.App{Const: option, Args: []types.Type{b}},
	})
	a = env.NewGenericVar()
	env.Declare("some", &types.Arrow{
		Args:   []types.Type{a},
		Return: &types.App{Const: option, Args: []types.Type{a}},
	})
	a, b = env.NewGenericVar(), env.NewGenericVar()
	env.Declare("option_ap", &types.Arrow{
		Args: []types.Type{
			&types.App{Const: option, Args: []types.Type{&types.Arrow{Args: []types.Type{a}, Return: b}}},
			&types.App{Const: option, Args: []types.Type{a}},
		},
		Return: &types.App{Const: option, Args: []types.Type{b}},
	})
	a, b = env.NewGenericVar(), env.NewGenericVar()
	env.Declare("option_bind", &types.Arrow{
		Args: []types.Type{
			&types.App{Const: option, Args: []types.Type{a}},
			&types.Arrow{Args: []types.Type{a}, Return: &types.App{Const: option, Args: []types.Type{b}}},
		},
		Return: &types.App{Const: option, Args: []types.Type{b}},
	})
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

	env.Declare("itoa", &types.Arrow{Args: []types.Type{&types.Const{"int"}}, Return: &types.Const{"string"}})
	env.Declare("someintoption", &types.App{Const: option, Args: []types.Type{&types.Const{"int"}}})

	var expr ast.Expr = &ast.Call{
		Func: &ast.Var{Name: "fmap"},
		Args: []ast.Expr{&ast.Var{Name: "itoa"}, &ast.Var{Name: "someintoption"}},
	}
	ty, err := ctx.Infer(expr, env)
	if err != nil {
		t.Fatal(err)
	}
	typeString := types.TypeString(ty)
	if typeString != "Option[string]" {
		t.Fatalf("type: %s", typeString)
	}

	expr = &ast.Call{
		Func: &ast.Var{Name: "(>>=)"},
		Args: []ast.Expr{
			&ast.Var{Name: "someintoption"},
			&ast.Func{
				ArgNames: []string{"i"},
				Body: &ast.Call{
					Func: &ast.Var{Name: "pure"},
					Args: []ast.Expr{
						&ast.Call{Func: &ast.Var{Name: "itoa"}, Args: []ast.Expr{&ast.Var{Name: "i"}}},
					},
				},
			},
		},
	}
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
	env.Declare("ref_map", &types.Arrow{
		Args: []types.Type{
			&types.Arrow{Args: []types.Type{a}, Return: b},
			types.NewRef(a),
		},
		Return: types.NewRef(b),
	})
	if _, err := env.DeclareInstance(Functor, types.RefType, map[string]string{"fmap": "ref_map"}); err != nil {
		t.Fatal(err)
	}
	env.Declare("someintref", types.NewRef(&types.Const{"int"}))
	expr = &ast.Call{
		Func: &ast.Var{Name: "fmap"},
		Args: []ast.Expr{&ast.Var{Name: "itoa"}, &ast.Var{Name: "someintref"}},
	}
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
	env.Declare("bad_map", &types.Arrow{
		Args: []types.Type{
			&types.Arrow{Args: []types.Type{a}, Return: b},
			&types.App{Const: &types.Const{"A"}, Args: []types.Type{a}},
		},
		Return: &types.App{Const: &types.Const{"B"}, Args: []types.Type{b}},
	})
	if _, err = env.DeclareInstance(Functor, &types.Const{"A"}, map[string]string{"fmap": "bad_map"}); err == nil {
		t.Fatalf("expected invalid-instance error")
	}

	// bad applicative (Vec is not declared as a Functor instance)
	a = env.NewGenericVar()
	env.Declare("new_vec", &types.Arrow{
		Args:   []types.Type{a},
		Return: &types.App{Const: &types.Const{"Vec"}, Args: []types.Type{a}},
	})
	a, b = env.NewGenericVar(), env.NewGenericVar()
	env.Declare("vec_ap", &types.Arrow{
		Args: []types.Type{
			&types.App{Const: &types.Const{"Vec"}, Args: []types.Type{&types.Arrow{Args: []types.Type{a}, Return: b}}},
			&types.App{Const: &types.Const{"Vec"}, Args: []types.Type{a}},
		},
		Return: &types.App{Const: &types.Const{"Vec"}, Args: []types.Type{b}},
	})
	if _, err := env.DeclareInstance(Applicative, &types.Const{"Vec"}, map[string]string{"pure": "new_vec", "(<*>)": "vec_ap"}); err == nil {
		t.Fatalf("expected invalid-instance error")
	}
}

func TestConstraints(t *testing.T) {
	env := NewTypeEnv(nil)
	ctx := NewContext()

	// type hierarchy

	intType := &types.Const{"int"}
	shortType := &types.Const{"short"}
	floatType := &types.Const{"float"}
	boolType := &types.Const{"bool"}

	intVecType := &types.App{
		Const: &types.Const{"vec"},
		Args:  []types.Type{intType},
	}

	Add, err := env.DeclareTypeClass("Add", func(param *types.Var) types.MethodSet {
		return types.MethodSet{
			"+": &types.Arrow{Args: []types.Type{param, param}, Return: param},
		}
	})
	if err != nil {
		t.Fatal(err)
	}
	adderVecType := &types.App{
		Const: &types.Const{"vec"},
		Args:  []types.Type{env.NewQualifiedVar(types.InstanceConstraint{Add})},
	}
	if types.TypeString(adderVecType) != "Add 'a => vec['a]" {
		t.Fatalf("invalid vec string: %s", types.TypeString(adderVecType))
	}

	Int, err := env.DeclareTypeClass("Int", func(param *types.Var) types.MethodSet {
		return types.MethodSet{
			"&": &types.Arrow{Args: []types.Type{param, param}, Return: param},
		}
	}, Add) // implements/subsumes Add
	if err != nil {
		t.Fatal(err)
	}

	env.Declare("int_add", &types.Arrow{Args: []types.Type{intType, intType}, Return: intType})
	env.Declare("int_and", &types.Arrow{Args: []types.Type{intType, intType}, Return: intType})
	env.Declare("short_add", &types.Arrow{Args: []types.Type{shortType, shortType}, Return: shortType})
	env.Declare("short_and", &types.Arrow{Args: []types.Type{shortType, shortType}, Return: shortType})
	env.Declare("float_add", &types.Arrow{Args: []types.Type{floatType, floatType}, Return: floatType})
	env.Declare("vec_add", &types.Arrow{Args: []types.Type{adderVecType, adderVecType}, Return: adderVecType})
	env.Declare("int_vec_add", &types.Arrow{Args: []types.Type{intVecType, intVecType}, Return: intVecType})
	env.Declare("bad_bool_add", &types.Arrow{Args: []types.Type{boolType, intType}, Return: boolType})

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
	env.Declare("someintvec", &types.App{
		Const: &types.Const{"vec"},
		Args:  []types.Type{intType},
	})
	env.Declare("somefloatvec", &types.App{
		Const: &types.Const{"vec"},
		Args:  []types.Type{floatType},
	})
	env.Declare("someboolvec", &types.App{
		Const: &types.Const{"vec"},
		Args:  []types.Type{boolType},
	})

	// method-instance lookups

	call := &ast.Call{
		Func: &ast.Var{Name: "+"},
		Args: []ast.Expr{&ast.Var{Name: "someint"}, &ast.Var{Name: "someint"}},
	}
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

	var addWrapper ast.Expr = &ast.Func{
		ArgNames: []string{"x", "y"},
		Body: &ast.Call{
			Func: &ast.Var{Name: "+"},
			Args: []ast.Expr{&ast.Var{Name: "x"}, &ast.Var{Name: "y"}},
		},
	}

	ty, err := ctx.Infer(addWrapper, env)
	if err != nil {
		t.Fatal(err)
	}
	typeString := types.TypeString(ty)
	if typeString != "Add 'a => ('a, 'a) -> 'a" {
		t.Fatalf("type: %s", typeString)
	}

	var expr ast.Expr = &ast.Call{
		Func: addWrapper,
		Args: []ast.Expr{&ast.Var{Name: "someint"}, &ast.Var{Name: "someint"}},
	}
	ty, err = ctx.Infer(expr, env)
	if err != nil {
		t.Fatal(err)
	}
	if tt, ok := ty.(*types.Const); !ok || tt.Name != "int" {
		t.Fatalf("expected int return, found " + tt.Name)
	}

	expr = &ast.Call{
		Func: addWrapper,
		Args: []ast.Expr{&ast.Var{Name: "someshort"}, &ast.Var{Name: "someshort"}},
	}
	ty, err = ctx.Infer(expr, env)
	if err != nil {
		t.Fatal(err)
	}
	if tt, ok := ty.(*types.Const); !ok || tt.Name != "short" {
		t.Fatalf("expected short return, found " + tt.Name)
	}

	expr = &ast.Call{
		Func: addWrapper,
		Args: []ast.Expr{&ast.Var{Name: "somefloat"}, &ast.Var{Name: "somefloat"}},
	}
	ty, err = ctx.Infer(expr, env)
	if err != nil {
		t.Fatal(err)
	}
	if tt, ok := ty.(*types.Const); !ok || tt.Name != "float" {
		t.Fatalf("expected float return, found " + tt.Name)
	}

	expr = &ast.Call{
		Func: addWrapper,
		Args: []ast.Expr{&ast.Var{Name: "someint"}, &ast.Var{Name: "somefloat"}},
	}
	ty, err = ctx.Infer(expr, env)
	if err == nil {
		t.Fatalf("expected constraint error")
	}

	expr = &ast.Call{
		Func: addWrapper,
		Args: []ast.Expr{&ast.Var{Name: "somebool"}, &ast.Var{Name: "somebool"}},
	}
	ty, err = ctx.Infer(expr, env)
	if err == nil {
		t.Fatalf("expected constraint error")
	}

	expr = &ast.Call{
		Func: addWrapper,
		Args: []ast.Expr{&ast.Var{Name: "someintvec"}, &ast.Var{Name: "someintvec"}},
	}
	ty, err = ctx.Infer(expr, env)
	if err != nil {
		t.Fatal(err)
	}
	typeString = types.TypeString(ty)
	if typeString != "vec[int]" {
		t.Fatalf("type: %s", typeString)
	}

	expr = &ast.Call{
		Func: addWrapper,
		Args: []ast.Expr{&ast.Var{Name: "somefloatvec"}, &ast.Var{Name: "somefloatvec"}},
	}
	ty, err = ctx.Infer(expr, env)
	if err != nil {
		t.Fatal(err)
	}
	typeString = types.TypeString(ty)
	if typeString != "vec[float]" {
		t.Fatalf("type: %s", typeString)
	}

	expr = &ast.Call{
		Func: addWrapper,
		Args: []ast.Expr{&ast.Var{Name: "someboolvec"}, &ast.Var{Name: "someboolvec"}},
	}
	ty, err = ctx.Infer(expr, env)
	if err == nil {
		t.Fatalf("expected constraint error")
	}

	expr = &ast.Call{
		Func: &ast.Var{Name: "&"},
		Args: []ast.Expr{&ast.Var{Name: "someint"}, &ast.Var{Name: "someint"}},
	}
	ty, err = ctx.Infer(expr, env)
	if err != nil {
		t.Fatal(err)
	}
	if tt, ok := ty.(*types.Const); !ok || tt.Name != "int" {
		t.Fatalf("expected int return, found " + tt.Name)
	}

	expr = &ast.Call{
		Func: &ast.Var{Name: "&"},
		Args: []ast.Expr{&ast.Var{Name: "someshort"}, &ast.Var{Name: "someshort"}},
	}
	ty, err = ctx.Infer(expr, env)
	if err != nil {
		t.Fatal(err)
	}
	if tt, ok := ty.(*types.Const); !ok || tt.Name != "short" {
		t.Fatalf("expected short return, found " + tt.Name)
	}

	expr = &ast.Call{
		Func: &ast.Var{Name: "&"},
		Args: []ast.Expr{&ast.Var{Name: "somefloat"}, &ast.Var{Name: "somefloat"}},
	}
	_, err = ctx.Infer(expr, env)
	if err == nil {
		t.Fatalf("expected constraint error")
	}

	expr = &ast.Call{
		Func: &ast.Var{Name: "&"},
		Args: []ast.Expr{&ast.Var{Name: "someint"}, &ast.Var{Name: "someshort"}},
	}
	_, err = ctx.Infer(expr, env)
	if err == nil {
		t.Fatalf("expected constraint error")
	}

	addWrapper = &ast.Func{
		ArgNames: []string{"x"},
		Body: &ast.Call{
			Func: &ast.Var{Name: "+"},
			Args: []ast.Expr{&ast.Var{Name: "x"}, &ast.Var{Name: "someint"}},
		},
	}
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

	env.Declare("add", &types.Arrow{
		Args:   []types.Type{&types.Const{"int"}, &types.Const{"int"}},
		Return: &types.Const{"int"},
	})
	A := env.NewGenericVar()
	env.Declare("if", &types.Arrow{
		Args:   []types.Type{&types.Const{"bool"}, A, A},
		Return: A,
	})
	env.Declare("newbool", &types.Arrow{Return: &types.Const{"bool"}})

	newbool := &ast.Call{Func: &ast.Var{Name: "newbool"}}
	f := &ast.Var{Name: "f"}
	g := &ast.Var{Name: "g"}
	h := &ast.Var{Name: "h"}
	id := &ast.Var{Name: "id"}
	x := &ast.Var{Name: "x"}

	expr := &ast.LetGroup{
		Vars: []ast.LetBinding{
			{
				Var: "id",
				Value: &ast.Func{
					ArgNames: []string{"x"},
					Body:     x,
				},
			},
			{
				Var: "f",
				Value: &ast.Func{
					ArgNames: []string{"x"},
					Body: &ast.Call{
						Func: &ast.Var{Name: "if"},
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
									Func: &ast.Var{Name: "add"},
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
						Func: &ast.Var{Name: "if"},
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

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

	env.Add("add", &types.Arrow{
		Args:   []types.Type{&types.Const{"int"}, &types.Const{"int"}},
		Return: &types.Const{"int"},
	})
	A := env.NewGenericVar()
	env.Add("if", &types.Arrow{
		Args:   []types.Type{&types.Const{"bool"}, A, A},
		Return: A,
	})
	env.Add("newbool", &types.Arrow{Return: &types.Const{"bool"}})

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
	t.Logf("expr: %s", exprString)

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
	t.Logf("type: %s", typeString)
}

func TestVariantMatch(t *testing.T) {
	env := NewTypeEnv(nil)
	ctx := NewContext()

	env.Add("add", &types.Arrow{
		Args:   []types.Type{&types.Const{"int"}, &types.Const{"int"}},
		Return: &types.Const{"int"},
	})
	env.Add("newint", &types.Arrow{Return: &types.Const{"int"}})
	env.Add("newbool", &types.Arrow{Return: &types.Const{"bool"}})

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
	t.Logf("expr: %s", exprString)

	ty, err := ctx.Infer(fnExpr, env)
	if err != nil {
		t.Fatal(err)
	}

	typeString := types.TypeString(ty)
	if typeString != "([a : int, b : int, c : 'a], int) -> int" {
		t.Fatalf("type: %s", typeString)
	}
	t.Logf("type: %s", typeString)

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
	t.Logf("expr: %s", exprString)

	ty, err = ctx.Infer(callExpr, env)
	if err != nil {
		t.Fatal(err)
	}

	typeString = types.TypeString(ty)
	if typeString != "int" {
		t.Fatalf("type: %s", typeString)
	}
	t.Logf("type: %s", typeString)
}

func TestKindConstraints(t *testing.T) {
	env := NewTypeEnv(nil)
	ctx := NewContext()

	intType := &types.Const{"int"}
	floatType := &types.Const{"float"}
	boolType := &types.Const{"bool"}

	Num := env.NewQualifiedVar(env.NewUnion("Num", intType, floatType))

	env.Add("newbool", &types.Arrow{Return: boolType})
	env.Add("+", &types.Arrow{
		Args:   []types.Type{Num, Num},
		Return: Num,
	})

	newbool := &ast.Call{Func: &ast.Var{Name: "newbool"}}

	var expr ast.Expr = &ast.Func{
		ArgNames: []string{"x", "y"},
		Body: &ast.Call{
			Func: &ast.Var{Name: "+"},
			Args: []ast.Expr{&ast.Var{Name: "x"}, &ast.Var{Name: "y"}},
		},
	}

	ty, err := ctx.Infer(expr, env)
	if err != nil {
		t.Fatal(err)
	}

	typeString := types.TypeString(ty)
	if typeString != "Num 'a => ('a, 'a) -> 'a" {
		t.Fatalf("type: %s", typeString)
	}
	t.Logf("type: %s", typeString)

	expr = &ast.Call{
		Func: expr,
		Args: []ast.Expr{newbool, newbool},
	}

	ty, err = ctx.Infer(expr, env)
	if err == nil {
		t.Fatalf("expected invalid kind error")
	}
	t.Logf("Passed check for kind-constraint error: %v", err)
}

func TestMutuallyRecursiveLet(t *testing.T) {
	env := NewTypeEnv(nil)
	ctx := NewContext()

	env.Add("add", &types.Arrow{
		Args:   []types.Type{&types.Const{"int"}, &types.Const{"int"}},
		Return: &types.Const{"int"},
	})
	A := env.NewGenericVar()
	env.Add("if", &types.Arrow{
		Args:   []types.Type{&types.Const{"bool"}, A, A},
		Return: A,
	})
	env.Add("newbool", &types.Arrow{Return: &types.Const{"bool"}})

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
	t.Logf("expr: %s", exprString)

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
	t.Logf("type: %s", typeString)
}

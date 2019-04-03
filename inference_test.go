package poly

import (
	"testing"

	"github.com/wdamron/poly/ast"
	"github.com/wdamron/poly/types"
)

func TestRecursiveLet(t *testing.T) {
	inf := NewInference()
	env := NewTypeEnvBuilder()
	env.Set("add", &types.Arrow{
		Args:   []types.Type{&types.Const{"int"}, &types.Const{"int"}},
		Return: &types.Const{"int"},
	})
	env.Set("if", &types.Arrow{
		Args:   []types.Type{&types.Const{"bool"}, &types.Const{"int"}, &types.Const{"int"}},
		Return: &types.Const{"int"},
	})
	env.Set("newbool", &types.Arrow{Return: &types.Const{"bool"}})

	expr := &ast.Func{
		ArgNames: []string{"x"},
		Body: &ast.Let{
			Var: "f",
			Value: &ast.Func{
				ArgNames: []string{"x"},
				Body: &ast.Call{
					Func: &ast.Var{"if"},
					Args: []ast.Expr{
						&ast.Call{Func: &ast.Var{"newbool"}},
						&ast.Var{"x"},
						&ast.Call{
							Func: &ast.Var{"f"},
							Args: []ast.Expr{&ast.Call{
								Func: &ast.Var{"add"},
								Args: []ast.Expr{&ast.Var{"x"}, &ast.Var{"x"}},
							}},
						}},
				},
			},
			Body: &ast.Call{
				Func: &ast.Var{"f"},
				Args: []ast.Expr{&ast.Var{"x"}},
			},
		},
	}

	exprString := ast.ExprString(expr)
	if exprString != "fun x -> let f = fun x -> if(newbool(), x, f(add(x, x))) in f(x)" {
		t.Fatalf("expr: %s", exprString)
	}
	t.Logf("expr: %s", exprString)

	ty, err := inf.ExprType(env.Build(), expr)
	if err != nil {
		t.Fatal(err)
	}

	typeString := types.TypeString(ty)
	if typeString != "int -> int" {
		t.Fatalf("type: %s", typeString)
	}
	t.Logf("type: %s", typeString)
}

func TestMutuallyRecursiveLet(t *testing.T) {
	inf := NewInference()
	env := NewTypeEnvBuilder()
	env.Set("add", &types.Arrow{
		Args:   []types.Type{&types.Const{"int"}, &types.Const{"int"}},
		Return: &types.Const{"int"},
	})
	env.Set("if", &types.Arrow{
		Args:   []types.Type{&types.Const{"bool"}, &types.Const{"int"}, &types.Const{"int"}},
		Return: &types.Const{"int"},
	})
	env.Set("newbool", &types.Arrow{Return: &types.Const{"bool"}})

	expr := &ast.Func{
		ArgNames: []string{"x"},
		Body: &ast.LetMulti{
			Vars: []ast.LabelValue{
				{
					Label: "f",
					Value: &ast.Func{
						ArgNames: []string{"x"},
						Body: &ast.Call{
							Func: &ast.Var{"if"},
							Args: []ast.Expr{
								&ast.Call{Func: &ast.Var{"newbool"}},
								&ast.Var{"x"},
								&ast.Call{
									Func: &ast.Var{"g"},
									Args: []ast.Expr{&ast.Call{
										Func: &ast.Var{"add"},
										Args: []ast.Expr{&ast.Var{"x"}, &ast.Var{"x"}},
									}},
								}},
						},
					},
				},
				{
					Label: "g",
					Value: &ast.Func{
						ArgNames: []string{"x"},
						Body: &ast.Call{
							Func: &ast.Var{"if"},
							Args: []ast.Expr{
								&ast.Call{Func: &ast.Var{"newbool"}},
								&ast.Var{"x"},
								&ast.Call{
									Func: &ast.Var{"f"},
									Args: []ast.Expr{&ast.Call{
										Func: &ast.Var{"add"},
										Args: []ast.Expr{&ast.Var{"x"}, &ast.Var{"x"}},
									}},
								}},
						},
					},
				},
				{
					Label: "h",
					Value: &ast.Func{
						ArgNames: []string{"x"},
						Body: &ast.Call{
							Func: &ast.Var{"f"},
							Args: []ast.Expr{&ast.Var{"x"}},
						},
					},
				},
			},
			Body: &ast.Call{
				Func: &ast.Var{"h"},
				Args: []ast.Expr{&ast.Var{"x"}},
			},
		},
	}

	exprString := ast.ExprString(expr)
	if exprString != "fun x -> let f = fun x -> if(newbool(), x, g(add(x, x))) and g = fun x -> if(newbool(), x, f(add(x, x))) and h = fun x -> f(x) in h(x)" {
		t.Fatalf("expr: %s", exprString)
	}
	t.Logf("expr: %s", exprString)

	ty, err := inf.ExprType(env.Build(), expr)
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
	inf := NewInference()
	env := NewTypeEnvBuilder()
	env.Set("add", &types.Arrow{
		Args:   []types.Type{&types.Const{"int"}, &types.Const{"int"}},
		Return: &types.Const{"int"},
	})
	env.Set("newint", &types.Arrow{Return: &types.Const{"int"}})
	env.Set("newbool", &types.Arrow{Return: &types.Const{"bool"}})

	fnExpr := &ast.Func{
		ArgNames: []string{"x", "y"},
		Body: &ast.Match{
			Value: &ast.Var{"x"},
			Cases: []ast.MatchCase{
				{"a", "i", &ast.Call{
					Func: &ast.Var{"add"},
					Args: []ast.Expr{&ast.Var{"i"}, &ast.Var{"i"}},
				}},
				{"b", "i", &ast.Call{
					Func: &ast.Var{"add"},
					Args: []ast.Expr{&ast.Var{"i"}, &ast.Var{"i"}},
				}},
				{"c", "_", &ast.Var{"y"}},
			},
		},
	}

	exprString := ast.ExprString(fnExpr)
	if exprString != "fun x y -> match x { :a i -> add(i, i) | :b i -> add(i, i) | :c _ -> y }" {
		t.Fatalf("expr: %s", exprString)
	}
	t.Logf("expr: %s", exprString)

	ty, err := inf.ExprType(env.Build(), fnExpr)
	if err != nil {
		t.Fatal(err)
	}

	typeString := types.TypeString(ty)
	if typeString != "forall[a] ([a : int, b : int, c : a], int) -> int" {
		t.Fatalf("type: %s", typeString)
	}
	t.Logf("type: %s", typeString)

	// Call:

	newint := &ast.Call{Func: &ast.Var{"newint"}}
	newbool := &ast.Call{Func: &ast.Var{"newbool"}}

	callExpr := &ast.Call{
		Func: fnExpr,
		Args: []ast.Expr{
			&ast.Variant{Label: "c", Value: newbool},
			newint,
		},
	}

	exprString = ast.ExprString(callExpr)
	if exprString != "(fun x y -> match x { :a i -> add(i, i) | :b i -> add(i, i) | :c _ -> y })(:c newbool(), newint())" {
		t.Fatalf("expr: %s", exprString)
	}
	t.Logf("expr: %s", exprString)

	ty, err = inf.ExprType(env.Build(), callExpr)
	if err != nil {
		t.Fatal(err)
	}

	typeString = types.TypeString(ty)
	if typeString != "int" {
		t.Fatalf("type: %s", typeString)
	}
	t.Logf("type: %s", typeString)
}

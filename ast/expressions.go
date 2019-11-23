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

package ast

import (
	"github.com/wdamron/poly/types"
)

// Expr is the base for all expressions.
type Expr interface {
	// Name of the syntax-type of the expression.
	ExprName() string
	// Type returns an inferred type of an expression. Expression types are only available after type-inference.
	Type() types.Type
}

var (
	_ Expr = (*Literal)(nil)
	_ Expr = (*Var)(nil)
	_ Expr = (*Pipe)(nil)
	_ Expr = (*Call)(nil)
	_ Expr = (*Func)(nil)
	_ Expr = (*Let)(nil)
	_ Expr = (*LetGroup)(nil)
	_ Expr = (*RecordSelect)(nil)
	_ Expr = (*RecordExtend)(nil)
	_ Expr = (*RecordRestrict)(nil)
	_ Expr = (*RecordEmpty)(nil)
	_ Expr = (*Variant)(nil)
	_ Expr = (*Match)(nil)
)

// Semi-opaque literal value
type Literal struct {
	// Syntax is a string representation of the literal value. The syntax will be printed when the literal is printed.
	Syntax string
	// Using may contain identifiers which will be looked up in the type-environment when the type is constructed.
	Using []string
	// Construct should produce a type at the given binding-level. The constructed type may include
	// types derived from variables which are already in scope (retrieved from the type-environment).
	Construct func(env types.TypeEnv, level int, using []types.Type) (types.Type, error)
	inferred  types.Type
}

// Returns the syntax of e.
func (e *Literal) ExprName() string { return e.Syntax }

// Get the inferred (or assigned) type of e.
func (e *Literal) Type() types.Type { return types.RealType(e.inferred) }

// Assign a type to e. Type assignments should occur indirectly, during inference.
func (e *Literal) SetType(t types.Type) { e.inferred = t }

// Variable
type Var struct {
	Name     string
	inferred types.Type
}

// "Var"
func (e *Var) ExprName() string { return "Var" }

// Get the inferred (or assigned) type of e.
func (e *Var) Type() types.Type { return types.RealType(e.inferred) }

// Assign a type to e. Type assignments should occur indirectly, during inference.
func (e *Var) SetType(t types.Type) { e.inferred = t }

// Application: `f(x)`
type Call struct {
	Func         Expr
	Args         []Expr
	inferred     types.Type
	inferredFunc *types.Arrow
}

// "Call"
func (e *Call) ExprName() string { return "Call" }

// Get the inferred (or assigned) type of e.
func (e *Call) Type() types.Type { return types.RealType(e.inferred) }

// Assign a type to e. Type assignments should occur indirectly, during inference.
func (e *Call) SetType(t types.Type) { e.inferred = t }

// Get the inferred (or assigned) function/method called in e.
func (e *Call) FuncType() *types.Arrow { return e.inferredFunc }

// Assign the function/method called in e. Type assignments should occur indirectly, during inference.
func (e *Call) SetFuncType(t *types.Arrow) { e.inferredFunc = t }

// Abstraction: `fn (x, y) -> x`
type Func struct {
	ArgNames []string
	Body     Expr
	inferred *types.Arrow
}

// "Func"
func (e *Func) ExprName() string { return "Func" }

// Get the inferred (or assigned) type of e.
func (e *Func) Type() types.Type { return types.RealType(e.inferred) }

// Assign a type to e. Type assignments should occur indirectly, during inference.
func (e *Func) SetType(ft *types.Arrow) { e.inferred = ft }

// Get the inferred (or assigned) type of an argument of e.
func (e *Func) ArgType(index int) types.Type { return types.RealType(e.inferred.Args[index]) }

// Get the inferred (or assigned) return type of e.
func (e *Func) RetType() types.Type { return types.RealType(e.inferred.Return) }

// Let-binding: `let a = 1 in e`
type Let struct {
	Var   string
	Value Expr
	Body  Expr
}

// "Let"
func (e *Let) ExprName() string { return "Let" }

// Get the inferred (or assigned) type of e.
func (e *Let) Type() types.Type { return e.Body.Type() }

// Grouped let-bindings: `let a = 1 and b = 2 in e`
type LetGroup struct {
	Vars []LetBinding
	Body Expr
}

// "LetGroup"
func (e *LetGroup) ExprName() string { return "LetGroup" }

// Get the inferred (or assigned) type of e.
func (e *LetGroup) Type() types.Type { return e.Body.Type() }

// Paired identifier and value
type LetBinding struct {
	Var   string
	Value Expr
}

// Get the inferred (or assigned) type of e.
func (e *LetBinding) Type() types.Type { return e.Value.Type() }

// Selecting value of label: `r.a`
type RecordSelect struct {
	Record   Expr
	Label    string
	inferred types.Type
}

// "RecordSelect"
func (e *RecordSelect) ExprName() string { return "RecordSelect" }

// Get the inferred (or assigned) type of e.
func (e *RecordSelect) Type() types.Type { return types.RealType(e.inferred) }

// Assign a type to e. Type assignments should occur indirectly, during inference.
func (e *RecordSelect) SetType(t types.Type) { e.inferred = t }

// Extending record: `{a = 1, b = 2 | r}`
type RecordExtend struct {
	Record   Expr
	Labels   []LabelValue
	inferred *types.Record
}

// "RecordExtend"
func (e *RecordExtend) ExprName() string { return "RecordExtend" }

// Get the inferred (or assigned) type of e.
func (e *RecordExtend) Type() types.Type { return types.RealType(e.inferred) }

// Assign a type to e. Type assignments should occur indirectly, during inference.
func (e *RecordExtend) SetType(rt *types.Record) { e.inferred = rt }

// Paired label and value
type LabelValue struct {
	Label string
	Value Expr
}

// Get the inferred (or assigned) type of e.
func (e *LabelValue) Type() types.Type { return e.Value.Type() }

// Deleting label: `{r - a}`
type RecordRestrict struct {
	Record   Expr
	Label    string
	inferred *types.Record
}

// "RecordRestrict"
func (e *RecordRestrict) ExprName() string { return "RecordRestrict" }

// Get the inferred (or assigned) type of e.
func (e *RecordRestrict) Type() types.Type { return types.RealType(e.inferred) }

// Assign a type to e. Type assignments should occur indirectly, during inference.
func (e *RecordRestrict) SetType(rt *types.Record) { e.inferred = rt }

// Empty record: `{}`
type RecordEmpty struct {
	inferred *types.Record
}

// "RecordEmpty"
func (e *RecordEmpty) ExprName() string { return "RecordEmpty" }

// Get the inferred (or assigned) type of e.
func (e *RecordEmpty) Type() types.Type { return types.RealType(e.inferred) }

// Assign a type to e. Type assignments should occur indirectly, during inference.
func (e *RecordEmpty) SetType(rt *types.Record) { e.inferred = rt }

// Tagged (ad-hoc) variant: `:X a`
type Variant struct {
	Label string
	Value Expr
}

// "Variant"
func (e *Variant) ExprName() string { return "Variant" }

// Get the inferred (or assigned) type of e.
func (e *Variant) Type() types.Type { return e.Value.Type() }

// Pattern-matching case expression over tagged (ad-hoc) variant-types:
//
//  match e {
//      :X a -> expr1
//    | :Y b -> expr2
//    |  ...
//    | z -> default_expr (optional)
//  }
type Match struct {
	Value    Expr
	Cases    []MatchCase
	Default  *MatchCase
	inferred types.Type
}

// "Match"
func (e *Match) ExprName() string { return "Match" }

// Get the inferred (or assigned) type of e.
func (e *Match) Type() types.Type { return types.RealType(e.inferred) }

// Assign a type to e. Type assignments should occur indirectly, during inference.
func (e *Match) SetType(t types.Type) { e.inferred = t }

// Case expression within Match: `:X a -> expr1`
type MatchCase struct {
	Label   string
	Var     string
	Value   Expr
	varType types.Type
}

// Get the inferred (or assigned) type of e.
func (e *MatchCase) Type() types.Type { return e.Value.Type() }

// Get the inferred (or assigned) variant-type of e.
func (e *MatchCase) VariantType() types.Type { return types.RealType(e.varType) }

// Assign a variant-type to e. Type assignments should occur indirectly, during inference.
func (e *MatchCase) SetVariantType(t types.Type) { e.varType = t }

// Pipeline: `pipe $ = xs |> fmap($, fn (x) -> to_y(x)) |> fmap($, fn (y) -> to_z(y))`
type Pipe struct {
	Source   Expr
	As       string
	Sequence []Expr
	inferred types.Type
}

// "Pipe"
func (e *Pipe) ExprName() string { return "Pipe" }

// Get the inferred (or assigned) type of e.
func (e *Pipe) Type() types.Type { return types.RealType(e.inferred) }

// Assign a type to e. Type assignments should occur indirectly, during inference.
func (e *Pipe) SetType(t types.Type) { e.inferred = t }

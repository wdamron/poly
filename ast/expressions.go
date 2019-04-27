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

func (e *Var) ExprName() string            { return "Var" }
func (e *Call) ExprName() string           { return "Call" }
func (e *Func) ExprName() string           { return "Func" }
func (e *Let) ExprName() string            { return "Let" }
func (e *LetGroup) ExprName() string       { return "LetGroup" }
func (e *RecordSelect) ExprName() string   { return "RecordSelect" }
func (e *RecordExtend) ExprName() string   { return "RecordExtend" }
func (e *RecordRestrict) ExprName() string { return "RecordRestrict" }
func (e *RecordEmpty) ExprName() string    { return "RecordEmpty" }
func (e *Variant) ExprName() string        { return "Variant" }
func (e *Match) ExprName() string          { return "Match" }

// Variable
type Var struct {
	Name     string
	inferred types.Type
}

func (e *Var) Type() types.Type     { return types.RealType(e.inferred) }
func (e *Var) SetType(t types.Type) { e.inferred = t }

// Application
type Call struct {
	Func     Expr
	Args     []Expr
	inferred types.Type
}

func (e *Call) Type() types.Type     { return types.RealType(e.inferred) }
func (e *Call) SetType(t types.Type) { e.inferred = t }

// Abstraction
type Func struct {
	ArgNames []string
	Body     Expr
	inferred *types.Arrow
}

func (e *Func) Type() types.Type             { return types.RealType(e.inferred) }
func (e *Func) SetType(ft *types.Arrow)      { e.inferred = ft }
func (e *Func) ArgType(index int) types.Type { return types.RealType(e.inferred.Args[index]) }
func (e *Func) RetType() types.Type          { return types.RealType(e.inferred.Return) }

// Let-binding: `let a = 1 in e`
type Let struct {
	Var   string
	Value Expr
	Body  Expr
}

func (e *Let) Type() types.Type { return e.Body.Type() }

// Grouped let-bindings: `let a = 1 and b = 2 in e`
type LetGroup struct {
	Vars []LetBinding
	Body Expr
}

func (e *LetGroup) Type() types.Type { return e.Body.Type() }

type LetBinding struct {
	Var   string
	Value Expr
}

func (e *LetBinding) Type() types.Type { return e.Value.Type() }

// Selecting value of label: `r.a`
type RecordSelect struct {
	Record   Expr
	Label    string
	inferred types.Type
}

func (e *RecordSelect) Type() types.Type     { return types.RealType(e.inferred) }
func (e *RecordSelect) SetType(t types.Type) { e.inferred = t }

// Extending record: `{a = 1, b = 2 | r}`
type RecordExtend struct {
	Record   Expr
	Labels   []LabelValue
	inferred *types.Record
}

func (e *RecordExtend) Type() types.Type         { return types.RealType(e.inferred) }
func (e *RecordExtend) SetType(rt *types.Record) { e.inferred = rt }

type LabelValue struct {
	Label string
	Value Expr
}

func (e *LabelValue) Type() types.Type { return e.Value.Type() }

// Deleting label: `{r - a}`
type RecordRestrict struct {
	Record   Expr
	Label    string
	inferred *types.Record
}

func (e *RecordRestrict) Type() types.Type         { return types.RealType(e.inferred) }
func (e *RecordRestrict) SetType(rt *types.Record) { e.inferred = rt }

// Empty record: `{}`
type RecordEmpty struct {
	inferred *types.Record
}

func (e *RecordEmpty) Type() types.Type         { return types.RealType(e.inferred) }
func (e *RecordEmpty) SetType(rt *types.Record) { e.inferred = rt }

// New variant value: `:X a`
type Variant struct {
	Label string
	Value Expr
}

func (e *Variant) Type() types.Type { return e.Value.Type() }

// Pattern-matching case expression:
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

func (e *Match) Type() types.Type     { return types.RealType(e.inferred) }
func (e *Match) SetType(t types.Type) { e.inferred = t }

// Case expression within Match: `:X a -> expr1`
type MatchCase struct {
	Label   string
	Var     string
	Value   Expr
	varType types.Type
}

func (e *MatchCase) Type() types.Type            { return e.Value.Type() }
func (e *MatchCase) VariantType() types.Type     { return types.RealType(e.varType) }
func (e *MatchCase) SetVariantType(t types.Type) { e.varType = t }

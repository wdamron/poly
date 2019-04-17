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

type Expr interface {
	ExprName() string
}

func (v *Var) ExprName() string            { return "Var" }
func (c *Call) ExprName() string           { return "Call" }
func (f *Func) ExprName() string           { return "Func" }
func (l *Let) ExprName() string            { return "Let" }
func (l *LetGroup) ExprName() string       { return "LetGroup" }
func (r *RecordSelect) ExprName() string   { return "RecordSelect" }
func (r *RecordExtend) ExprName() string   { return "RecordExtend" }
func (r *RecordRestrict) ExprName() string { return "RecordRestrict" }
func (r RecordEmpty) ExprName() string     { return "RecordEmpty" }
func (v *Variant) ExprName() string        { return "Variant" }
func (m *Match) ExprName() string          { return "Match" }
func (c *MatchCase) ExprName() string      { return "MatchCase" }

// Variable
type Var struct {
	Name string
}

// Application
type Call struct {
	Func Expr
	Args []Expr
}

// Abstraction
type Func struct {
	ArgNames []string
	Body     Expr
}

// Let-binding: `let a = 1 in e`
type Let struct {
	Var   string
	Value Expr
	Body  Expr
}

// Grouped let-bindings: `let a = 1 and b = 2 in e`
type LetGroup struct {
	Vars []LetBinding
	Body Expr
}

type LetBinding struct {
	Var   string
	Value Expr
}

// Selecting value of label: `r.a`
type RecordSelect struct {
	Record Expr
	Label  string
}

// Extending record: `{a = 1, b = 2 | r}`
type RecordExtend struct {
	Record Expr
	Labels []LabelValue
}

type LabelValue struct {
	Label string
	Value Expr
}

// Deleting label: `{r - a}`
type RecordRestrict struct {
	Record Expr
	Label  string
}

// Empty record: `{}`
type RecordEmpty struct{}

// New variant value: `:X a`
type Variant struct {
	Label string
	Value Expr
}

// Pattern-matching case expression:
//
//  match e {
//      :X a -> expr1
//    | :Y b -> expr2
//    |  ...
//    | z -> default_expr (optional)
//  }
type Match struct {
	Value   Expr
	Cases   []MatchCase
	Default *MatchCase
}

// Case expression within Match: `:X a -> expr1`
type MatchCase struct {
	Label string
	Var   string
	Value Expr
}

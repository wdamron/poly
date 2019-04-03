package ast

type Expr interface {
	ExprName() string
}

func (v *Var) ExprName() string            { return "Var" }
func (c *Call) ExprName() string           { return "Call" }
func (f *Func) ExprName() string           { return "Func" }
func (l *Let) ExprName() string            { return "Let" }
func (l *LetMulti) ExprName() string       { return "LetMulti" }
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

// Multiple let-binding: `let a = 1 and b = 2 in e`
type LetMulti struct {
	Vars []LabelValue
	Body Expr
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

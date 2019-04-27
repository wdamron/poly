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

func CopyExpr(e Expr) Expr {
	switch e := e.(type) {
	case *Var:
		return &Var{e.Name, e.inferred}

	case *Call:
		args := make([]Expr, len(e.Args))
		for i, arg := range e.Args {
			args[i] = CopyExpr(arg)
		}
		return &Call{CopyExpr(e.Func), args, e.inferred}

	case *Func:
		return &Func{e.ArgNames, CopyExpr(e.Body), e.inferred}

	case *Let:
		return &Let{e.Var, CopyExpr(e.Value), CopyExpr(e.Body)}

	case *LetGroup:
		vars := make([]LetBinding, len(e.Vars))
		for i, v := range e.Vars {
			vars[i] = LetBinding{v.Var, CopyExpr(v.Value)}
		}
		return &LetGroup{vars, CopyExpr(e.Body)}

	case *RecordSelect:
		return &RecordSelect{CopyExpr(e.Record), e.Label, e.inferred}

	case *RecordExtend:
		labels := make([]LabelValue, len(e.Labels))
		for i, v := range e.Labels {
			labels[i] = LabelValue{v.Label, CopyExpr(v.Value)}
		}
		record := e.Record
		if record == nil {
			record = &RecordEmpty{}
		} else {
			record = CopyExpr(record)
		}
		return &RecordExtend{record, labels, e.inferred}

	case *RecordRestrict:
		return &RecordRestrict{CopyExpr(e.Record), e.Label, e.inferred}

	case *RecordEmpty:
		return &RecordEmpty{e.inferred}

	case *Variant:
		return &Variant{e.Label, CopyExpr(e.Value)}

	case *Match:
		cases := make([]MatchCase, len(e.Cases))
		for i, v := range e.Cases {
			cases[i] = MatchCase{v.Label, v.Var, CopyExpr(v.Value), v.varType}
		}
		defaultCase := e.Default
		if defaultCase != nil {
			defaultCase = &MatchCase{defaultCase.Label, defaultCase.Var, CopyExpr(defaultCase.Value), defaultCase.varType}
		}
		return &Match{CopyExpr(e.Value), cases, defaultCase, e.inferred}
	}
	panic("unknown expression type: " + e.ExprName())
}

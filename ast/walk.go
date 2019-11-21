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

func WalkExpr(e Expr, f func(Expr)) {
	switch e := e.(type) {
	case *Var, *Literal, *RecordEmpty:
		f(e)

	case *Call:
		f(e)
		for _, arg := range e.Args {
			WalkExpr(arg, f)
		}

	case *Func:
		f(e)
		WalkExpr(e.Body, f)

	case *Pipe:
		f(e.Source)
		for _, step := range e.Sequence {
			WalkExpr(step, f)
		}

	case *Let:
		f(e)
		WalkExpr(e.Value, f)
		WalkExpr(e.Body, f)

	case *LetGroup:
		f(e)
		for _, v := range e.Vars {
			WalkExpr(v.Value, f)
		}
		WalkExpr(e.Body, f)

	case *RecordSelect:
		f(e)
		WalkExpr(e.Record, f)

	case *RecordExtend:
		f(e)
		for _, v := range e.Labels {
			WalkExpr(v.Value, f)
		}
		WalkExpr(e.Record, f)

	case *RecordRestrict:
		f(e)
		WalkExpr(e.Record, f)

	case *Variant:
		f(e)
		WalkExpr(e.Value, f)

	case *Match:
		f(e)
		for _, v := range e.Cases {
			WalkExpr(v.Value, f)
		}
		if e.Default != nil {
			WalkExpr(e.Default.Value, f)
		}

	case nil:

	default:
		panic("unknown expression type: " + e.ExprName())
	}
}

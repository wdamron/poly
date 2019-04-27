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
	"sort"
	"strings"
)

func ExprString(e Expr) string {
	var sb strings.Builder
	exprString(&sb, false, e)
	return sb.String()
}

func exprString(sb *strings.Builder, simple bool, e Expr) {
	switch e := e.(type) {
	case *Var:
		sb.WriteString(e.Name)

	case *Call:
		exprString(sb, true, e.Func)
		sb.WriteByte('(')
		for i, arg := range e.Args {
			if i > 0 {
				sb.WriteString(", ")
			}
			exprString(sb, false, arg)
		}
		sb.WriteByte(')')

	case *Func:
		if simple {
			sb.WriteByte('(')
		}
		sb.WriteString("fun ")
		if len(e.ArgNames) == 0 {
			sb.WriteString("()")
		}
		for i, arg := range e.ArgNames {
			if i > 0 {
				sb.WriteByte(' ')
			}
			sb.WriteString(arg)
		}
		sb.WriteString(" -> ")
		exprString(sb, false, e.Body)
		if simple {
			sb.WriteByte(')')
		}

	case *Let:
		if simple {
			sb.WriteByte('(')
		}
		sb.WriteString("let ")
		sb.WriteString(e.Var)
		sb.WriteString(" = ")
		exprString(sb, false, e.Value)
		sb.WriteString(" in ")
		exprString(sb, false, e.Body)
		if simple {
			sb.WriteByte(')')
		}

	case *LetGroup:
		if simple {
			sb.WriteByte('(')
		}
		sb.WriteString("let ")
		for i, v := range e.Vars {
			if i > 0 {
				sb.WriteString(" and ")
			}
			sb.WriteString(v.Var)
			sb.WriteString(" = ")
			exprString(sb, false, v.Value)
		}
		sb.WriteString(" in ")
		exprString(sb, false, e.Body)
		if simple {
			sb.WriteByte(')')
		}

	case *RecordEmpty:
		sb.WriteString("{}")

	case *RecordSelect:
		exprString(sb, true, e.Record)
		sb.WriteByte('.')
		sb.WriteString(e.Label)

	case *RecordRestrict:
		sb.WriteByte('{')
		exprString(sb, false, e.Record)
		sb.WriteString(" - ")
		sb.WriteString(e.Label)
		sb.WriteByte('}')

	case *RecordExtend:
		sb.WriteByte('{')
		labels := make([]LabelValue, len(e.Labels))
		copy(labels, e.Labels)
		sort.Slice(labels, func(i, j int) bool {
			return labels[i].Label < labels[j].Label
		})
		for i, label := range labels {
			if i > 0 {
				sb.WriteString(", ")
			}
			sb.WriteString(label.Label)
			sb.WriteString(" = ")
			exprString(sb, false, label.Value)
		}
		switch e.Record.(type) {
		case *RecordEmpty:
		case nil:
		default:
			sb.WriteString(" | ")
			exprString(sb, false, e.Record)
		}
		sb.WriteByte('}')

	case *Variant:
		if simple {
			sb.WriteByte('(')
		}
		sb.WriteByte(':')
		sb.WriteString(e.Label)
		sb.WriteByte(' ')
		exprString(sb, true, e.Value)
		if simple {
			sb.WriteByte(')')
		}

	case *Match:
		sb.WriteString("match ")
		exprString(sb, false, e.Value)
		sb.WriteString(" {")
		for i, c := range e.Cases {
			if i > 0 {
				sb.WriteString(" |")
			}
			sb.WriteString(" :")
			sb.WriteString(c.Label)
			sb.WriteByte(' ')
			sb.WriteString(c.Var)
			sb.WriteString(" -> ")
			exprString(sb, false, c.Value)
		}
		if e.Default != nil {
			sb.WriteString(" | ")
			sb.WriteString(e.Default.Var)
			sb.WriteString(" -> ")
			exprString(sb, false, e.Default.Value)
		}
		sb.WriteString(" }")
	}
}

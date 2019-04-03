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
	switch et := e.(type) {
	case *Var:
		sb.WriteString(et.Name)

	case *Call:
		exprString(sb, true, et.Func)
		sb.WriteByte('(')
		for i, arg := range et.Args {
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
		for i, arg := range et.ArgNames {
			if i > 0 {
				sb.WriteByte(' ')
			}
			sb.WriteString(arg)
		}
		sb.WriteString(" -> ")
		exprString(sb, false, et.Body)
		if simple {
			sb.WriteByte(')')
		}

	case *Let:
		if simple {
			sb.WriteByte('(')
		}
		sb.WriteString("let ")
		sb.WriteString(et.Var)
		sb.WriteString(" = ")
		exprString(sb, false, et.Value)
		sb.WriteString(" in ")
		exprString(sb, false, et.Body)
		if simple {
			sb.WriteByte(')')
		}

	case *LetMulti:
		if simple {
			sb.WriteByte('(')
		}
		sb.WriteString("let ")
		for i, v := range et.Vars {
			if i > 0 {
				sb.WriteString(" and ")
			}
			sb.WriteString(v.Label)
			sb.WriteString(" = ")
			exprString(sb, false, v.Value)
		}
		sb.WriteString(" in ")
		exprString(sb, false, et.Body)
		if simple {
			sb.WriteByte(')')
		}

	case RecordEmpty:
		sb.WriteString("{}")

	case *RecordSelect:
		exprString(sb, true, et.Record)
		sb.WriteByte('.')
		sb.WriteString(et.Label)

	case *RecordRestrict:
		sb.WriteByte('{')
		exprString(sb, false, et.Record)
		sb.WriteString(" - ")
		sb.WriteString(et.Label)
		sb.WriteByte('}')

	case *RecordExtend:
		sb.WriteByte('{')
		labels := make([]LabelValue, len(et.Labels))
		copy(labels, et.Labels)
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
		switch et.Record.(type) {
		case RecordEmpty:
		default:
			sb.WriteString(" | ")
			exprString(sb, false, et.Record)
		}
		sb.WriteByte('}')

	case *Variant:
		if simple {
			sb.WriteByte('(')
		}
		sb.WriteByte(':')
		sb.WriteString(et.Label)
		sb.WriteByte(' ')
		exprString(sb, true, et.Value)
		if simple {
			sb.WriteByte(')')
		}

	case *Match:
		sb.WriteString("match ")
		exprString(sb, false, et.Value)
		sb.WriteString(" {")
		for i, c := range et.Cases {
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
		if et.Default != nil {
			sb.WriteString(" | ")
			sb.WriteString(et.Default.Var)
			sb.WriteString(" -> ")
			exprString(sb, false, et.Default.Value)
		}
		sb.WriteString(" }")
	}
}

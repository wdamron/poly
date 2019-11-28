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
	"strconv"
	"strings"
)

func ExprString(e Expr) string {
	var sb strings.Builder
	exprString(&sb, false, e)
	return sb.String()
}

func exprString(sb *strings.Builder, simple bool, e Expr) {
	switch e := e.(type) {
	case *Literal:
		sb.WriteString(e.Syntax)

	case *Var:
		sb.WriteString(e.Name)

	case *Deref:
		if simple {
			sb.WriteByte('(')
		}
		sb.WriteByte('*')
		exprString(sb, true, e.Ref)
		if simple {
			sb.WriteByte(')')
		}

	case *DerefAssign:
		if simple {
			sb.WriteByte('(')
		}
		sb.WriteByte('*')
		exprString(sb, true, e.Ref)
		sb.WriteString(" = ")
		exprString(sb, false, e.Value)
		if simple {
			sb.WriteByte(')')
		}

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
		sb.WriteString("fn ")
		if len(e.ArgNames) == 0 {
			sb.WriteString("()")
		} else {
			sb.WriteByte('(')
			for i, arg := range e.ArgNames {
				if i > 0 {
					sb.WriteString(", ")
				}
				sb.WriteString(arg)
			}
			sb.WriteByte(')')
		}
		sb.WriteString(" -> ")
		exprString(sb, false, e.Body)
		if simple {
			sb.WriteByte(')')
		}

	case *ControlFlow:
		if simple {
			sb.WriteByte('(')
		}
		controlFlow(sb, e)
		if simple {
			sb.WriteByte(')')
		}

	case *Pipe:
		if len(e.Sequence) == 0 {
			exprString(sb, simple, e.Source)
			return
		}
		if simple {
			sb.WriteByte('(')
		}
		sb.WriteString("pipe ")
		sb.WriteString(e.As)
		sb.WriteString(" = ")
		exprString(sb, true, e.Source)
		for _, step := range e.Sequence {
			sb.WriteString(" |> ")
			exprString(sb, true, step)
		}
		if simple {
			sb.WriteByte(')')
		}

	case *Let:
		if simple {
			sb.WriteByte('(')
		}
		sb.WriteString("let ")
		bindingString(sb, e.Var, e.Value)
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
			bindingString(sb, v.Var, v.Value)
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
			bindingString(sb, label.Label, label.Value)
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

func bindingString(sb *strings.Builder, label string, value Expr) {
	fn, ok := value.(*Func)
	if !ok {
		sb.WriteString(label)
		sb.WriteString(" = ")
		exprString(sb, false, value)
		return
	}

	sb.WriteString(label)
	if len(fn.ArgNames) == 0 {
		sb.WriteString("()")
	} else {
		sb.WriteByte('(')
		for i, arg := range fn.ArgNames {
			if i > 0 {
				sb.WriteString(", ")
			}
			sb.WriteString(arg)
		}
		sb.WriteByte(')')
	}

	sb.WriteString(" = ")
	exprString(sb, false, fn.Body)
}

func printSeq(sb *strings.Builder, seq []Expr) {
	if len(seq) == 0 {
		sb.WriteString("{}")
		return
	}
	if len(seq) == 1 {
		exprString(sb, false, seq[0])
		return
	}
	sb.WriteByte('{')
	for i, sub := range seq {
		if i > 0 {
			sb.WriteString("; ")
		}
		exprString(sb, false, sub)
	}
	sb.WriteByte('}')
}

var _blockLabels [128]string

func init() {
	for i := range _blockLabels {
		_blockLabels[i] = "L" + strconv.Itoa(i)
	}
}

func printBlockLabel(sb *strings.Builder, index int) {
	switch index {
	case ControlFlowEntryIndex:
		sb.WriteString("entry")
	case ControlFlowReturnIndex:
		sb.WriteString("return")
	default:
		if index < len(_blockLabels) {
			sb.WriteString(_blockLabels[index])
			return
		}
		sb.WriteByte('L')
		sb.WriteString(strconv.Itoa(index))
	}
}

func controlFlow(sb *strings.Builder, e *ControlFlow) {
	sb.WriteString(e.Name)
	sb.WriteByte('(')
	for i, name := range e.Locals {
		if i > 0 {
			sb.WriteString(", ")
		}
		sb.WriteString(name)
	}
	sb.WriteString(") {")
	var blocks []Block
	if len(e.Entry.Sequence) != 0 {
		blocks = append(blocks, e.Entry)
	}
	if len(e.Return.Sequence) != 0 {
		blocks = append(blocks, e.Return)
	}
	if len(e.Blocks) != 0 {
		blocks = append(blocks, e.Blocks...)
	}
	if len(blocks) != 0 {
		for i, block := range blocks {
			if len(block.Sequence) == 0 {
				continue
			}
			if i > 0 {
				sb.WriteString(", ")
			}
			printBlockLabel(sb, block.Index)
			sb.WriteString(" : ")
			printSeq(sb, block.Sequence)
		}
	}
	sb.WriteString("} in {")
	if len(e.Jumps) != 0 {
		jumps := SortJumps(e.Jumps)
		lastFrom := ControlFlowEntryIndex - 1
		for i, j := range jumps {
			if j.From == lastFrom {
				sb.WriteString(", ")
				printBlockLabel(sb, j.To)
			} else {
				if i > 0 {
					sb.WriteString("], ")
				}
				printBlockLabel(sb, j.From)
				sb.WriteString(" -> [")
				printBlockLabel(sb, j.To)
			}
			if i == len(jumps)-1 {
				sb.WriteByte(']')
			}
			lastFrom = j.From
		}
	}
	sb.WriteByte('}')
}

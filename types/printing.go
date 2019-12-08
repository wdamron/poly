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

package types

import (
	"sort"
	"strconv"
	"strings"
	"sync"
)

var printerPool = sync.Pool{
	New: func() interface{} {
		p := &typePrinter{
			idNames: make(map[uint]string, 16),
			preds:   make(map[uint][]string, 16),
		}
		p.order = p._order[:0]
		return p
	},
}

func newTypePrinter() *typePrinter { return printerPool.Get().(*typePrinter) }

func (p *typePrinter) Release() {
	for k := range p.idNames {
		delete(p.idNames, k)
	}
	for k := range p.preds {
		delete(p.preds, k)
	}
	p.order = p._order[:0]
	p.sb.Reset()
	printerPool.Put(p)
}

// TypeString returns a string representation of a Type.
func TypeString(t Type) string {
	p := newTypePrinter()
	typeString(p, false, t)
	if len(p.preds) == 0 {
		s := p.sb.String()
		p.Release()
		return s
	}

	order := p.order
	for id := range p.preds {
		order = append(order, id)
	}
	sort.Slice(order, func(i, j int) bool { return p.idNames[order[i]] < p.idNames[order[j]] })
	var sb strings.Builder
	multiplePreds := len(order) > 1 || len(p.preds[order[0]]) > 1
	if multiplePreds {
		sb.WriteByte('(')
	}
	for i, id := range order {
		if i > 0 {
			sb.WriteString(", ")
		}
		idName := p.idNames[id]
		for j, pred := range p.preds[id] {
			if j > 0 {
				sb.WriteString(", ")
			}
			sb.WriteString(pred)
			sb.WriteByte(' ')
			sb.WriteString(idName)
		}
	}
	if multiplePreds {
		sb.WriteByte(')')
	}

	sb.WriteString(" => ")
	sb.WriteString(p.sb.String())
	p.Release()
	return sb.String()
}

type typePrinter struct {
	idNames map[uint]string
	preds   map[uint][]string
	order   []uint
	_order  [16]uint
	sb      strings.Builder
}

var _names [128]string
var _unboundNames [128]string

func init() {
	for i := range _names {
		if i >= 26 {
			_names[i] = "'" + string(byte(97+i%26)) + strconv.Itoa(i/26)
		}
		_names[i] = "'" + string(byte(97+i%26))
	}
	for i := range _unboundNames {
		_unboundNames[i] = "'_" + strconv.Itoa(i)
	}
}

func getVarName(i uint) string {
	if i < uint(len(_names)) {
		return _names[i]
	}
	if i >= 26 {
		return "'" + string(byte(97+i%26)) + strconv.Itoa(int(i/26))
	}
	return "'" + string(byte(97+i%26))
}

func getUnboundVarName(i uint) string {
	if i < uint(len(_unboundNames)) {
		return _unboundNames[i]
	}
	return "'_" + strconv.Itoa(int(i))
}

func (p *typePrinter) nextName() string {
	return getVarName(uint(len(p.idNames)))
}

func typeString(p *typePrinter, simple bool, t Type) {
	switch t := t.(type) {
	case *Unit:
		p.sb.WriteString("()")

	case *Const:
		p.sb.WriteString(t.Name)

	case Size:
		p.sb.WriteString(strconv.Itoa(int(t)))

	case *Var:
		switch {
		case t.IsUnboundVar():
			if len(p.idNames) == 0 {
				p.idNames = make(map[uint]string)
			} else if name, ok := p.idNames[t.Id()]; ok {
				p.sb.WriteString(name)
				return
			}
			name := getUnboundVarName(t.Id())
			p.sb.WriteString(name)
			p.idNames[t.Id()] = name

		case t.IsLinkVar():
			typeString(p, simple, t.Link())

		case t.IsGenericVar():
			if len(p.idNames) == 0 {
				p.idNames = make(map[uint]string)
			} else if name, ok := p.idNames[t.Id()]; ok {
				p.sb.WriteString(name)
				return
			}
			name := p.nextName()
			p.idNames[t.Id()] = name
			p.sb.WriteString(name)
		}
		if len(t.constraints) == 0 && !t.IsWeakVar() && !t.IsRestrictedVar() {
			return
		}
		if p.preds != nil && len(p.preds[t.Id()]) > 0 {
			return
		} else if p.preds == nil {
			p.preds = make(map[uint][]string)
		}
		preds := make([]string, 0, len(t.constraints)+2) // space for [weak, size]
		if t.IsWeakVar() {
			preds = append(preds, "weak")
		}
		switch {
		case t.IsSizeVar():
			preds = append(preds, "size")
		case t.IsConstVar():
			preds = append(preds, "const")
		}
		for _, c := range t.constraints {
			preds = append(preds, c.TypeClass.Name)
		}
		p.preds[t.Id()] = preds

	case *RecursiveLink:
		typeString(p, false, t.Link())

	case *App:
		typeString(p, true, t.Const)
		if len(t.Params) == 0 {
			return
		}
		p.sb.WriteByte('[')
		for i, param := range t.Params {
			if i > 0 {
				p.sb.WriteString(", ")
			}
			typeString(p, false, param)
		}
		p.sb.WriteByte(']')

	case *Arrow:
		if simple {
			p.sb.WriteByte('(')
		}
		if len(t.Args) == 1 {
			typeString(p, true, t.Args[0])
			p.sb.WriteString(" -> ")
			typeString(p, false, t.Return)
		} else {
			p.sb.WriteByte('(')
			for i, arg := range t.Args {
				if i > 0 {
					p.sb.WriteString(", ")
				}
				typeString(p, false, arg)
			}
			p.sb.WriteString(") -> ")
			typeString(p, false, t.Return)
		}
		if simple {
			p.sb.WriteByte(')')
		}

	case *Method:
		arrow := t.TypeClass.Methods[t.Name]
		typeString(p, false, arrow)

	case *Record:
		p.sb.WriteByte('{')
		typeString(p, false, t.Row)
		p.sb.WriteByte('}')

	case *Variant:
		p.sb.WriteByte('[')
		typeString(p, false, t.Row)
		p.sb.WriteByte(']')

	case *RowEmpty: // nothing to print

	case *RowExtend:
		labels, row, err := FlattenRowType(t)
		if err != nil {
			p.sb.WriteString("<INVALID-ROW>")
			break
		}
		i := 0
		labels.Range(func(label string, ts TypeList) bool {
			if i > 0 {
				p.sb.WriteString(", ")
			}
			ts.Range(func(i int, t Type) bool {
				if i > 0 {
					p.sb.WriteString(", ")
				}
				p.sb.WriteString(label)
				p.sb.WriteString(" : ")
				typeString(p, false, t)
				return true
			})
			i++
			return true
		})
		rest := RealType(row)
		switch rest.(type) {
		case *RowEmpty: // nothing to print
		case *RowExtend:
			p.sb.WriteString("<INVALID-ROW>")
			break
		default:
			p.sb.WriteString(" | ")
			typeString(p, false, rest)
		}
	}
}

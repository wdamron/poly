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
)

// TypeString returns a string representation of a Type.
func TypeString(t Type) string {
	p := typePrinter{}
	typeString(&p, false, t)
	if len(p.kinds) == 0 {
		return p.sb.String()
	}

	order := make([]int, 0, len(p.kinds))
	for id := range p.kinds {
		order = append(order, id)
	}
	sort.Slice(order, func(i, j int) bool { return p.idNames[order[i]] < p.idNames[order[j]] })
	var sb strings.Builder
	multipleKinds := len(order) > 1 || len(p.kinds[order[0]]) > 1
	if multipleKinds {
		sb.WriteByte('(')
	}
	for i, id := range order {
		if i > 0 {
			sb.WriteString(", ")
		}
		idName := p.idNames[id]
		for j, k := range p.kinds[id] {
			if j > 0 {
				sb.WriteString(", ")
			}
			sb.WriteString(k.Name)
			sb.WriteByte(' ')
			sb.WriteString(idName)
		}
	}
	if multipleKinds {
		sb.WriteByte(')')
	}

	sb.WriteString(" => ")
	sb.WriteString(p.sb.String())
	return sb.String()
}

type typePrinter struct {
	idNames map[int]string
	kinds   map[int][]*Kind
	sb      strings.Builder
}

func (p *typePrinter) nextName() string {
	i := len(p.idNames)
	if i >= 26 {
		return string(byte(97+i%26)) + strconv.Itoa(i/26)
	}
	return "'" + string(byte(97+i%26))
}

func typeString(p *typePrinter, simple bool, t Type) {
	switch t := t.(type) {
	case *Const:
		p.sb.WriteString(t.Name)

	case *Var:
		switch {
		case t.IsUnboundVar():
			p.sb.WriteString("'_")
			p.sb.WriteString(strconv.Itoa(t.Id()))

		case t.IsLinkVar():
			typeString(p, simple, t.Link())

		case t.IsGenericVar():
			if len(p.idNames) == 0 {
				p.idNames = make(map[int]string)
			} else if name, ok := p.idNames[t.Id()]; ok {
				p.sb.WriteString(name)
				return
			}
			name := p.nextName()
			p.idNames[t.Id()] = name
			p.sb.WriteString(name)
		}
		if len(t.kinds) == 0 {
			return
		}
		if p.kinds == nil {
			p.kinds = map[int][]*Kind{
				t.Id(): t.kinds,
			}
		} else {
			if _, ok := p.kinds[t.Id()]; !ok {
				p.kinds[t.Id()] = t.kinds
			}
		}

	case *App:
		typeString(p, true, t.Func)
		p.sb.WriteByte('[')
		for i, arg := range t.Args {
			if i > 0 {
				p.sb.WriteString(", ")
			}
			typeString(p, false, arg)
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
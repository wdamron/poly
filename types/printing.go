package types

import (
	"sort"
	"strconv"
	"strings"
)

func TypeString(t Type) string {
	p := typePrinter{}
	typeString(&p, false, t)
	if p.counter == 0 {
		return p.sb.String()
	}
	names := make([]string, 0, len(p.ids))
	for _, name := range p.ids {
		names = append(names, name)
	}
	sort.Strings(names)
	var sb strings.Builder
	sb.WriteString("forall[")
	for i, name := range names {
		if i > 0 {
			sb.WriteByte(' ')
		}
		sb.WriteString(name)
	}
	sb.WriteString("] ")
	sb.WriteString(p.sb.String())
	return sb.String()
}

type typePrinter struct {
	ids     map[int]string
	sb      strings.Builder
	counter int
}

func (p *typePrinter) nextName() string {
	i := p.counter
	p.counter++
	if i >= 26 {
		return string(byte(97+i%26)) + strconv.Itoa(i/26)
	}
	return string(byte(97 + i%26))
}

func typeString(p *typePrinter, simple bool, t Type) {
	switch tt := t.(type) {
	case *Const:
		p.sb.WriteString(tt.Name)

	case *Var:
		switch tv := tt.Instance.(type) {
		case GenericVar:
			if len(p.ids) == 0 {
				p.ids = make(map[int]string)
			} else if name, ok := p.ids[tv.Id]; ok {
				p.sb.WriteString(name)
				return
			}
			name := p.nextName()
			p.ids[tv.Id] = name
			p.sb.WriteString(name)
		case UnboundVar:
			p.sb.WriteByte('_')
			p.sb.WriteString(strconv.Itoa(tv.Id))
		case LinkVar:
			typeString(p, simple, tv.Type)
		}

	case *App:
		typeString(p, true, tt.Func)
		p.sb.WriteByte('[')
		for i, arg := range tt.Args {
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
		if len(tt.Args) == 1 {
			typeString(p, true, tt.Args[0])
			p.sb.WriteString(" -> ")
			typeString(p, false, tt.Return)
		} else {
			p.sb.WriteByte('(')
			for i, arg := range tt.Args {
				if i > 0 {
					p.sb.WriteString(", ")
				}
				typeString(p, false, arg)
			}
			p.sb.WriteString(") -> ")
			typeString(p, false, tt.Return)
		}
		if simple {
			p.sb.WriteByte(')')
		}

	case *Record:
		p.sb.WriteByte('{')
		typeString(p, false, tt.Row)
		p.sb.WriteByte('}')

	case *Variant:
		p.sb.WriteByte('[')
		typeString(p, false, tt.Row)
		p.sb.WriteByte(']')

	case RowEmpty: // nothing to print

	case *RowExtend:
		labels, row, err := MatchRowType(tt)
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
		case RowEmpty: // nothing to print
		case *RowExtend:
			p.sb.WriteString("<INVALID-ROW>")
			break
		default:
			p.sb.WriteString(" | ")
			typeString(p, false, rest)
		}
	}
}

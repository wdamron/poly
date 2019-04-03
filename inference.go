package poly

import (
	"errors"

	"github.com/benbjohnson/immutable"
	"github.com/wdamron/poly/ast"
	"github.com/wdamron/poly/types"
)

var emptyMap = immutable.NewMap(nil)
var emptyList = immutable.NewList()

type Inference struct {
	currentId int
	err       error
	invalid   ast.Expr
}

func NewInference() *Inference { return &Inference{} }

func (ti *Inference) ExprType(env TypeEnv, expr ast.Expr) (types.Type, error) {
	if expr == nil {
		return nil, errors.New("Empty expression")
	}
	if env.m == nil {
		env.m = emptyMap
	}
	t, err := ti.infer(env.m, 0, expr)
	if err != nil {
		return t, err
	}
	return generalize(-1, t), nil
}

func (ti *Inference) Error() error          { return ti.err }
func (ti *Inference) InvalidExpr() ast.Expr { return ti.invalid }
func (ti *Inference) Reset()                { ti.currentId, ti.err, ti.invalid = 0, nil, nil }

func (ti *Inference) nextId() int {
	id := ti.currentId
	ti.currentId++
	return id
}

func (ti *Inference) newVar(level int) *types.Var {
	return &types.Var{Instance: types.UnboundVar{Id: ti.nextId(), Level: level}}
}

func (ti *Inference) newVars(level, count int) []types.Var {
	vars := make([]types.Var, count)
	for i := range vars {
		vars[i] = types.Var{Instance: types.UnboundVar{Id: ti.nextId(), Level: level}}
	}
	return vars
}

func (ti *Inference) newGenVar() *types.Var {
	return &types.Var{Instance: types.GenericVar{Id: ti.nextId()}}
}

func (ti *Inference) matchFuncType(argc int, t types.Type) (*types.Arrow, error) {
	switch tt := t.(type) {
	case *types.Arrow:
		if len(tt.Args) != argc {
			return tt, errors.New("Unexpected number of arguments")
		}
		return tt, nil

	case *types.Var:
		switch tvi := tt.Instance.(type) {
		case types.LinkVar:
			return ti.matchFuncType(argc, tvi.Type)
		case types.UnboundVar:
			args := make([]types.Type, argc)
			for i := 0; i < argc; i++ {
				args[i] = ti.newVar(tvi.Level)
			}
			ret := ti.newVar(tvi.Level)
			a := &types.Arrow{Args: args, Return: ret}
			tt.Set(types.LinkVar{Type: a})
			return a, nil
		}
	}

	return nil, errors.New("Unexpected function type")
}

func (ti *Inference) occursAdjustLevels(id, level int, t types.Type) error {
	switch tt := t.(type) {
	case *types.Var:
		switch ttv := tt.Instance.(type) {
		case types.LinkVar:
			return ti.occursAdjustLevels(id, level, ttv.Type)
		case types.GenericVar:
			return errors.New("Invalid occurrance check within generic type-variable")
		case types.UnboundVar:
			if ttv.Id == id {
				return errors.New("Invalid recursive types")
			}
			if ttv.Level > level {
				tt.Set(types.UnboundVar{Id: ttv.Id, Level: level})
			}
		}
		return nil

	case *types.App:
		if err := ti.occursAdjustLevels(id, level, tt.Func); err != nil {
			return err
		}
		for _, arg := range tt.Args {
			if err := ti.occursAdjustLevels(id, level, arg); err != nil {
				return err
			}
		}
		return nil

	case *types.Arrow:
		for _, arg := range tt.Args {
			if err := ti.occursAdjustLevels(id, level, arg); err != nil {
				return err
			}
		}
		return ti.occursAdjustLevels(id, level, tt.Return)

	case *types.Record:
		return ti.occursAdjustLevels(id, level, tt.Row)

	case *types.Variant:
		return ti.occursAdjustLevels(id, level, tt.Row)

	case *types.RowExtend:
		var err error
		tt.Labels.Range(func(label string, ts types.TypeList) bool {
			ts.Range(func(i int, t types.Type) bool {
				err = ti.occursAdjustLevels(id, level, t)
				return err == nil
			})
			return err == nil
		})
		if err != nil {
			return err
		}
		return ti.occursAdjustLevels(id, level, tt.Row)

	default:
		return nil
	}
}

func (ti *Inference) unify(a, b types.Type) error {
	if a == b {
		return nil
	}

	if ta, ok := a.(*types.Var); ok {
		switch tva := ta.Instance.(type) {
		case types.LinkVar:
			return ti.unify(tva.Type, b)
		case types.UnboundVar:
			if tb, ok := b.(*types.Var); ok {
				if tvb, ok := tb.Instance.(types.UnboundVar); ok && tva.Id == tvb.Id {
					return errors.New("Cannot unify pair of unbound type-variables")
				}
			}
			if err := ti.occursAdjustLevels(tva.Id, tva.Level, b); err != nil {
				return err
			}
			ta.Set(types.LinkVar{Type: b})
			return nil
		default:
			return errors.New("Failed to unify type variable with type")
		}
	}

	if tb, ok := b.(*types.Var); ok {
		return ti.unify(tb, a)
	}

	switch ta := a.(type) {
	case *types.Const:
		if tb, ok := b.(*types.Const); ok && ta.Name == tb.Name {
			return nil
		}

	case *types.App:
		tb, ok := b.(*types.App)
		if !ok {
			return errors.New("Failed to unify function call with type")
		}
		if err := ti.unify(ta.Func, tb.Func); err != nil {
			return err
		}
		if len(ta.Args) != len(tb.Args) {
			return errors.New("Cannot unify function calls with differing arity")
		}
		for i := range ta.Args {
			if err := ti.unify(ta.Args[i], tb.Args[i]); err != nil {
				return err
			}
		}
		return nil

	case *types.Arrow:
		tb, ok := b.(*types.Arrow)
		if !ok {
			return errors.New("Failed to unify arrow with type")
		}
		if len(ta.Args) != len(tb.Args) {
			return errors.New("Cannot unify arrows with differing arity")
		}
		for i := range ta.Args {
			if err := ti.unify(ta.Args[i], tb.Args[i]); err != nil {
				return err
			}
		}
		if err := ti.unify(ta.Return, tb.Return); err != nil {
			return err
		}
		return nil

	case *types.Record:
		if tb, ok := b.(*types.Record); ok {
			return ti.unify(ta.Row, tb.Row)
		}

	case *types.Variant:
		if tb, ok := b.(*types.Variant); ok {
			return ti.unify(ta.Row, tb.Row)
		}

	case *types.RowExtend:
		if tb, ok := b.(*types.RowExtend); ok {
			return ti.unifyRows(ta, tb)
		}

	case types.RowEmpty:
		if _, ok := b.(types.RowEmpty); ok {
			return nil
		}
	}

	return errors.New("Failed to unify with unknown type")
}

func (ti *Inference) unifyLists(a, b types.TypeList) (xa, xb types.TypeList, err error) {
	i := 0
	n := a.Len()
	if b.Len() < n {
		n = b.Len()
	}
	for i < n {
		va, vb := a.Get(i), b.Get(i)
		if err := ti.unify(va, vb); err != nil {
			return types.EmptyTypeList, types.EmptyTypeList, err
		}
		i++
	}
	return a.Slice(i, a.Len()), b.Slice(i, b.Len()), nil
}

func (ti *Inference) unifyRows(a, b types.Type) error {
	ma, ra, err := types.MatchRowType(a)
	if err != nil {
		return err
	}
	mb, rb, err := types.MatchRowType(b)
	if err != nil {
		return err
	}

	xa, xb := types.NewTypeMapBuilder(), types.NewTypeMapBuilder()
	ia, ib := ma.Iterator(), mb.Iterator()

UnifyLabels:
	for {
		switch {
		case ia.Done() && ib.Done():
			break UnifyLabels
		case ia.Done():
			for !ib.Done() {
				xa.Set(ib.Next())
			}
			break UnifyLabels
		case ib.Done():
			for !ia.Done() {
				xb.Set(ia.Next())
			}
			break UnifyLabels
		}

		la, va := ia.Peek()
		lb, vb := ib.Peek()
		switch {
		case la > lb:
			xa.Set(lb, vb)
			ib.Next()
		case la < lb:
			xb.Set(la, va)
			ia.Next()
		default:
			ua, ub, err := ti.unifyLists(va, vb)
			if err != nil {
				return err
			}
			if ua.Len() > 0 {
				xb.Set(la, ua)
			}
			if ub.Len() > 0 {
				xa.Set(lb, ub)
			}
			ia.Next()
			ib.Next()
		}
	}

	za, zb := xa.Len() == 0, xb.Len() == 0
	switch {
	case za && zb:
		return ti.unify(ra, rb)
	case za && !zb:
		return ti.unify(rb, &types.RowExtend{Row: ra, Labels: xb.Build()})
	case !za && zb:
		return ti.unify(ra, &types.RowExtend{Row: rb, Labels: xa.Build()})
	default:
		switch rat := ra.(type) {
		case types.RowEmpty:
			// will result in an error:
			return ti.unify(ra, &types.RowExtend{Row: ti.newVar(0), Labels: xa.Build()})
		case *types.Var:
			ub, ok := rat.Instance.(types.UnboundVar)
			if !ok {
				return errors.New("Invalid state while unifying type variables for rows")
			}
			tv := ti.newVar(ub.Level)
			if err := ti.unify(rb, &types.RowExtend{Row: tv, Labels: xb.Build()}); err != nil {
				return err
			}
			if _, ok := rat.Instance.(types.LinkVar); ok {
				return errors.New("Invalid recursive row-types")
			}
			return ti.unify(ra, &types.RowExtend{Row: tv, Labels: xa.Build()})
		}
	}

	return errors.New("Invalid state while unifying rows")
}

func generalize(level int, t types.Type) types.Type {
	switch tt := t.(type) {
	case *types.Var:
		switch tvi := tt.Instance.(type) {
		case types.UnboundVar:
			if tvi.Level > level {
				return &types.Var{Instance: types.GenericVar{Id: tvi.Id}}
			}
			return tt
		case types.LinkVar:
			return generalize(level, tvi.Type)
		default:
			return tt
		}

	case *types.App:
		args := make([]types.Type, len(tt.Args))
		for i, arg := range tt.Args {
			args[i] = generalize(level, arg)
		}
		return &types.App{Func: generalize(level, tt.Func), Args: args}

	case *types.Arrow:
		args := make([]types.Type, len(tt.Args))
		for i, arg := range tt.Args {
			args[i] = generalize(level, arg)
		}
		return &types.Arrow{Args: args, Return: generalize(level, tt.Return)}

	case *types.Record:
		return &types.Record{Row: generalize(level, tt.Row)}

	case *types.Variant:
		return &types.Variant{Row: generalize(level, tt.Row)}

	case *types.RowExtend:
		m := tt.Labels
		mb := m.Builder()
		m.Range(func(label string, ts types.TypeList) bool {
			lb := ts.Builder()
			ts.Range(func(i int, t types.Type) bool {
				lb.Set(i, generalize(level, t))
				return true
			})
			mb.Set(label, lb.Build())
			return true
		})
		return &types.RowExtend{Row: generalize(level, tt.Row), Labels: mb.Build()}

	default:
		return tt
	}
}

func (ti *Inference) instantiate(lookup map[int]*types.Var, level int, t types.Type) types.Type {
	switch tt := t.(type) {
	case *types.Const:
		return tt

	case *types.Var:
		switch tvi := tt.Instance.(type) {
		case types.LinkVar:
			return ti.instantiate(lookup, level, tvi.Type)
		case types.GenericVar:
			if len(lookup) > 0 {
				if v, ok := lookup[tvi.Id]; ok {
					return v
				}
			} else {
				lookup = make(map[int]*types.Var)
			}
			v := ti.newVar(level)
			lookup[tvi.Id] = v
			return v
		case types.UnboundVar:
			return tt
		}

	case *types.App:
		args := make([]types.Type, len(tt.Args))
		for i, arg := range tt.Args {
			args[i] = ti.instantiate(lookup, level, arg)
		}
		return &types.App{Func: ti.instantiate(lookup, level, tt.Func), Args: args}

	case *types.Arrow:
		args := make([]types.Type, len(tt.Args))
		for i, arg := range tt.Args {
			args[i] = ti.instantiate(lookup, level, arg)
		}
		return &types.Arrow{Args: args, Return: ti.instantiate(lookup, level, tt.Return)}

	case *types.Record:
		return &types.Record{Row: ti.instantiate(lookup, level, tt.Row)}

	case *types.Variant:
		return &types.Variant{Row: ti.instantiate(lookup, level, tt.Row)}

	case types.RowEmpty:
		return tt

	case *types.RowExtend:
		m := tt.Labels
		mb := m.Builder()
		m.Range(func(label string, ts types.TypeList) bool {
			lb := ts.Builder()
			ts.Range(func(i int, t types.Type) bool {
				lb.Set(i, ti.instantiate(lookup, level, t))
				return true
			})
			mb.Set(label, lb.Build())
			return true
		})
		return &types.RowExtend{Row: ti.instantiate(lookup, level, tt.Row), Labels: mb.Build()}
	}
	return nil
}

func (ti *Inference) infer(env *immutable.Map, level int, e ast.Expr) (types.Type, error) {
	switch et := e.(type) {
	case *ast.Var:
		v, ok := env.Get(et.Name)
		if !ok {
			ti.invalid, ti.err = e, errors.New("Variable "+et.Name+" not found")
			return nil, ti.err
		}
		return ti.instantiate(nil, level, v.(types.Type)), nil

	case *ast.Func:
		args := make([]types.Type, len(et.ArgNames))
		fenv := immutable.NewMapBuilder(env)
		for i, name := range et.ArgNames {
			args[i] = ti.newVar(level)
			fenv.Set(name, args[i])
		}
		ret, err := ti.infer(fenv.Map(), level, et.Body)
		return &types.Arrow{Args: args, Return: ret}, err

	case *ast.Let:
		v := ti.newVar(level)
		t, err := ti.infer(env.Set(et.Var, v), level+1, et.Value)
		if err != nil {
			return nil, err
		}
		if err := ti.unify(v, t); err != nil {
			return nil, err
		}
		t = generalize(level, t)
		return ti.infer(env.Set(et.Var, t), level, et.Body)

	case *ast.LetMulti:
		tvs := ti.newVars(level, len(et.Vars))
		letenv := immutable.NewMapBuilder(env)
		for i, v := range et.Vars {
			letenv.Set(v.Label, &tvs[i])
		}
		env = letenv.Map()
		for i, v := range et.Vars {
			tv := &tvs[i]
			t, err := ti.infer(env, level+1, v.Value)
			if err != nil {
				return nil, err
			}
			if err := ti.unify(tv, t); err != nil {
				return nil, err
			}
			letenv.Set(v.Label, generalize(level, t))
		}
		return ti.infer(letenv.Map(), level, et.Body)

	case *ast.Call:
		ft, err := ti.infer(env, level, et.Func)
		if err != nil {
			return nil, err
		}
		arrow, err := ti.matchFuncType(len(et.Args), ft)
		if err != nil {
			ti.invalid, ti.err = e, err
			return nil, err
		}
		for i, arg := range et.Args {
			ta, err := ti.infer(env, level, arg)
			if err != nil {
				return nil, err
			}
			if err := ti.unify(arrow.Args[i], ta); err != nil {
				ti.invalid, ti.err = e, err
				return nil, err
			}
		}
		return arrow.Return, nil

	case ast.RecordEmpty:
		return &types.Record{Row: types.RowEmpty{}}, nil

	case *ast.RecordSelect:
		rowType := ti.newVar(level)
		labelType := ti.newVar(level)
		labels := types.SingletonTypeMap(et.Label, labelType)
		paramType := &types.Record{Row: &types.RowExtend{Row: rowType, Labels: labels}}
		recordType, err := ti.infer(env, level, et.Record)
		if err != nil {
			return nil, err
		}
		if err := ti.unify(paramType, recordType); err != nil {
			ti.invalid, ti.err = e, err
			return nil, err
		}
		return labelType, nil

	case *ast.RecordRestrict:
		rowType := ti.newVar(level)
		labelType := ti.newVar(level)
		labels := types.SingletonTypeMap(et.Label, labelType)
		paramType := &types.Record{Row: &types.RowExtend{Row: rowType, Labels: labels}}
		recordType, err := ti.infer(env, level, et.Record)
		if err != nil {
			return nil, err
		}
		if err := ti.unify(paramType, recordType); err != nil {
			ti.invalid, ti.err = e, err
			return nil, err
		}
		return &types.Record{Row: rowType}, nil

	case *ast.RecordExtend:
		mb := types.NewTypeMapBuilder()
		for _, label := range et.Labels {
			t, err := ti.infer(env, level, label.Value)
			if err != nil {
				return nil, err
			}
			mb.Set(label.Label, types.SingletonTypeList(t))
		}
		rowType := ti.newVar(level)
		recordType, err := ti.infer(env, level, et.Record)
		if err != nil {
			return nil, err
		}
		if err := ti.unify(&types.Record{Row: rowType}, recordType); err != nil {
			ti.invalid, ti.err = e, err
			return nil, err
		}
		return &types.Record{Row: &types.RowExtend{Row: rowType, Labels: mb.Build()}}, nil

	case *ast.Variant:
		rowType := ti.newVar(level)
		variantType := ti.newVar(level)
		t, err := ti.infer(env, level, et.Value)
		if err != nil {
			return nil, err
		}
		if err := ti.unify(variantType, t); err != nil {
			ti.invalid, ti.err = e, err
			return nil, err
		}
		labels := types.SingletonTypeMap(et.Label, variantType)
		return &types.Variant{Row: &types.RowExtend{Row: rowType, Labels: labels}}, nil

	case *ast.Match:
		if et.Default == nil {
			retType := ti.newVar(level)
			matchType, err := ti.infer(env, level, et.Value)
			casesRow, err := ti.inferCases(env, level, retType, types.RowEmpty{}, et.Cases)
			if err != nil {
				return nil, err
			}
			if err := ti.unify(matchType, &types.Variant{Row: casesRow}); err != nil {
				ti.invalid, ti.err = e, err
				return nil, err
			}
			return retType, nil
		}
		defaultType := ti.newVar(level)
		retType, err := ti.infer(env.Set(et.Default.Var, &types.Variant{Row: defaultType}), level, et.Default.Value)
		if err != nil {
			return nil, err
		}
		matchType, err := ti.infer(env, level, et.Value)
		if err != nil {
			return nil, err
		}
		casesRow, err := ti.inferCases(env, level, retType, defaultType, et.Cases)
		if err != nil {
			ti.invalid, ti.err = e, err
			return nil, err
		}
		if err := ti.unify(matchType, &types.Variant{Row: casesRow}); err != nil {
			ti.invalid, ti.err = e, err
			return nil, err
		}
		return retType, nil
	}

	ti.invalid, ti.err = e, errors.New("Unknown type")
	return nil, ti.err
}

func (ti *Inference) inferCases(env *immutable.Map, level int, retType, rowType types.Type, cases []ast.MatchCase) (types.Type, error) {
	casesRow := rowType
	extensions := make([]types.RowExtend, len(cases))
	for i := len(cases) - 1; i >= 0; i-- {
		c := cases[i]
		variantType := ti.newVar(level)
		t, err := ti.infer(env.Set(c.Var, variantType), level, c.Value)
		if err != nil {
			return nil, err
		}
		if err := ti.unify(retType, t); err != nil {
			return nil, err
		}
		labels := types.SingletonTypeMap(c.Label, variantType)
		extensions[i].Row, extensions[i].Labels = casesRow, labels
		casesRow = &extensions[i]
	}
	return casesRow, nil
}

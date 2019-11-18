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

package poly

import (
	"errors"

	"github.com/wdamron/poly/ast"
	"github.com/wdamron/poly/types"
)

func (ti *InferenceContext) infer(env *TypeEnv, level int, e ast.Expr) (ret types.Type, err error) {
	switch e := e.(type) {
	case *ast.Literal:
		t := ti.common.Instantiate(level, e.Construct(env, level))
		if ti.annotate {
			e.SetType(t)
		}
		return t, nil

	case *ast.Var:
		t := env.Lookup(e.Name)
		if t == nil {
			ti.invalid, ti.err = e, errors.New("Variable "+e.Name+" not is not defined")
			return nil, ti.err
		}
		t = ti.common.Instantiate(level, t)
		if ti.annotate {
			e.SetType(t)
		}
		return t, nil

	case *ast.Let:
		// Disallow self-references within non-function types:
		if _, isFunc := e.Value.(*ast.Func); !isFunc {
			t, err := ti.infer(env, level+1, e.Value)
			if err != nil {
				return nil, err
			}
			stashed := ti.common.Stash(env, e.Var)
			env.Types[e.Var] = types.GeneralizeAtLevel(level, t)
			t, err = ti.infer(env, level, e.Body)
			delete(env.Types, e.Var)
			ti.common.Unstash(env, stashed)
			return t, err
		}
		// Allow self-references within function types:
		tv := ti.common.VarTracker.New(level + 1)
		stashed := ti.common.Stash(env, e.Var)
		env.Types[e.Var] = tv
		t, err := ti.infer(env, level+1, e.Value)
		if err != nil {
			return nil, err
		}
		if err := ti.common.Unify(tv, t); err != nil {
			ti.invalid, ti.err = e, err
			return nil, err
		}
		env.Types[e.Var] = types.GeneralizeAtLevel(level, t)
		t, err = ti.infer(env, level, e.Body)
		delete(env.Types, e.Var)
		ti.common.Unstash(env, stashed)
		return t, err

	case *ast.LetGroup:
		if !ti.analyzed {
			if err := ti.analysis.Analyze(ti.rootExpr); err != nil {
				ti.invalid, ti.err, ti.analysis.Invalid = ti.analysis.Invalid, err, nil
				return nil, err
			}
			ti.analyzed = true
		}
		stashed := 0
		// Grouped let-bindings are sorted into strongly-connected components, then type-checked in dependency order:
		for _, scc := range ti.analysis.Sccs[ti.analysis.GroupNums[e]] {
			// Add fresh type-variables for bindings:
			vars := ti.common.VarTracker.NewList(level+1, len(scc))
			tv, tail := vars.Head(), vars.Tail()
			for _, bindNum := range scc {
				v := e.Vars[bindNum]
				stashed += ti.common.Stash(env, v.Var)
				env.Types[v.Var] = tv
				tv, tail = tail.Head(), tail.Tail()
			}
			// Infer types:
			tv, tail = vars.Head(), vars.Tail()
			for _, bindNum := range scc {
				v := e.Vars[bindNum]
				var isFunc bool
				// To prevent self-references within non-function types, stash/remove the type-variable:
				if _, isFunc = v.Value.(*ast.Func); !isFunc {
					exists := false
					for i := 0; i < stashed; i++ {
						existing := ti.common.EnvStash[len(ti.common.EnvStash)-(1+i)]
						if existing.Name == v.Var {
							env.Types[v.Var] = existing.Type
							exists = true
							break
						}
					}
					if !exists {
						delete(env.Types, v.Var)
					}
				}
				t, err := ti.infer(env, level+1, v.Value)
				if err != nil {
					return nil, err
				}
				if err := ti.common.Unify(tv, t); err != nil {
					ti.invalid, ti.err = e, err
					return nil, err
				}
				// Restore the previously stashed/removed type-variable:
				if !isFunc {
					env.Types[v.Var] = tv
				}
				tv, tail = tail.Head(), tail.Tail()
			}
			// Generalize types:
			tv, tail = vars.Head(), vars.Tail()
			for _, bindNum := range scc {
				v := e.Vars[bindNum]
				env.Types[v.Var] = types.GeneralizeAtLevel(level, tv)
				tv, tail = tail.Head(), tail.Tail()
			}
		}
		t, err := ti.infer(env, level, e.Body)
		for _, v := range e.Vars {
			delete(env.Types, v.Var)
		}
		ti.common.Unstash(env, stashed)
		return t, err

	case *ast.Func:
		args := make([]types.Type, len(e.ArgNames))
		stashed := 0
		vars := ti.common.VarTracker.NewList(level, len(e.ArgNames))
		tv, tail := vars.Head(), vars.Tail()
		for i, name := range e.ArgNames {
			stashed += ti.common.Stash(env, name)
			args[i] = tv
			env.Types[name] = tv
			tv, tail = tail.Head(), tail.Tail()
		}
		ret, err := ti.infer(env, level, e.Body)
		for _, name := range e.ArgNames {
			delete(env.Types, name)
		}
		ti.common.Unstash(env, stashed)
		t := &types.Arrow{Args: args, Return: ret}
		if ti.annotate {
			e.SetType(t)
		}
		return t, err

	case *ast.Call:
		ft, err := ti.infer(env, level, e.Func)
		if err != nil {
			return nil, err
		}

		args, ret, err := ti.matchFuncType(len(e.Args), ft)
		if err != nil {
			ti.invalid, ti.err = e, err
			return nil, err
		}
		for i, arg := range e.Args {
			ta, err := ti.infer(env, level, arg)
			if err != nil {
				return nil, err
			}
			if err := ti.common.Unify(args[i], ta); err != nil {
				ti.invalid, ti.err = e, err
				return nil, err
			}
		}
		if ti.annotate {
			arrow, _ := ft.(*types.Arrow)
			e.SetFuncType(arrow)
			e.SetType(ret)
		}
		return ret, nil

	case *ast.RecordEmpty:
		rt := &types.Record{Row: types.RowEmptyPointer}
		if ti.annotate {
			e.SetType(rt)
		}
		return rt, nil

	case *ast.RecordSelect:
		// label, rest := fresh(), fresh()
		// unify({ <label>: label | rest }, record)
		// -> label
		label, _, err := ti.splitRecord(env, level, e.Record, e.Label)
		if err != nil {
			ti.invalid, ti.err = e, err
			return nil, err
		}
		return label, nil

	case *ast.RecordRestrict:
		// label, rest := fresh(), fresh()
		// unify({ <label>: label | rest }, record)
		// -> rest
		_, rest, err := ti.splitRecord(env, level, e.Record, e.Label)
		if err != nil {
			ti.invalid, ti.err = e, err
			return nil, err
		}
		return rest, nil

	case *ast.RecordExtend:
		mb := types.NewTypeMapBuilder()
		for _, label := range e.Labels {
			t, err := ti.infer(env, level, label.Value)
			if err != nil {
				return nil, err
			}
			mb.Set(label.Label, types.SingletonTypeList(t))
		}
		rowType := ti.common.VarTracker.New(level)
		recordType, err := ti.infer(env, level, e.Record)
		if err != nil {
			return nil, err
		}
		if err := ti.common.Unify(&types.Record{Row: rowType}, recordType); err != nil {
			ti.invalid, ti.err = e, err
			return nil, err
		}
		rt := &types.Record{Row: &types.RowExtend{Row: rowType, Labels: mb.Build()}}
		if ti.annotate {
			e.SetType(rt)
		}
		return rt, nil

	case *ast.Variant:
		rowType := ti.common.VarTracker.New(level)
		variantType := ti.common.VarTracker.New(level)
		t, err := ti.infer(env, level, e.Value)
		if err != nil {
			return nil, err
		}
		if err := ti.common.Unify(variantType, t); err != nil {
			ti.invalid, ti.err = e, err
			return nil, err
		}
		labels := types.SingletonTypeMap(e.Label, variantType)
		vt := &types.Variant{Row: &types.RowExtend{Row: rowType, Labels: labels}}
		return vt, nil

	case *ast.Match:
		if e.Default == nil {
			retType := ti.common.VarTracker.New(level)
			matchType, err := ti.infer(env, level, e.Value)
			casesRow, err := ti.inferCases(env, level, retType, types.RowEmptyPointer, e.Cases)
			if err != nil {
				return nil, err
			}
			if err := ti.common.Unify(matchType, &types.Variant{Row: casesRow}); err != nil {
				ti.invalid, ti.err = e, err
				return nil, err
			}
			if ti.annotate {
				e.SetType(retType)
			}
			return retType, nil
		}
		defaultType := ti.common.VarTracker.New(level)
		stashed := ti.common.Stash(env, e.Default.Var)
		env.Types[e.Default.Var] = &types.Variant{Row: defaultType}
		retType, err := ti.infer(env, level, e.Default.Value)
		delete(env.Types, e.Default.Var)
		ti.common.Unstash(env, stashed)
		if err != nil {
			return nil, err
		}
		matchType, err := ti.infer(env, level, e.Value)
		if err != nil {
			return nil, err
		}
		casesRow, err := ti.inferCases(env, level, retType, defaultType, e.Cases)
		if err != nil {
			ti.invalid, ti.err = e, err
			return nil, err
		}
		if err := ti.common.Unify(matchType, &types.Variant{Row: casesRow}); err != nil {
			ti.invalid, ti.err = e, err
			return nil, err
		}
		if ti.annotate {
			e.SetType(retType)
		}
		return retType, nil
	}

	var exprName string
	if e != nil {
		exprName = "(" + e.ExprName() + ")"
	} else {
		exprName = "(nil)"
	}
	ti.invalid, ti.err = e, errors.New("Unhandled expression type "+exprName)
	return nil, ti.err
}

// label, rest := fresh(), fresh()
// unify({ <label>: label | rest }, record)
// -> (label, rest)
func (ti *InferenceContext) splitRecord(env *TypeEnv, level int, recordExpr ast.Expr, label string) (labelType types.Type, restType types.Type, err error) {
	rowType := ti.common.VarTracker.New(level)
	labelType = ti.common.VarTracker.New(level)
	labels := types.SingletonTypeMap(label, labelType)
	paramType := &types.Record{Row: &types.RowExtend{Row: rowType, Labels: labels}}
	var recordType types.Type
	recordType, err = ti.infer(env, level, recordExpr)
	if err != nil {
		return nil, nil, err
	}
	if err = ti.common.Unify(paramType, recordType); err != nil {
		return nil, nil, err
	}
	restType = &types.Record{Row: rowType}
	return
}

// If t is an unbound type-variable, instantiate a function with unbound type-variables for its arguments and return value;
// otherwise, ensure t has the correct argument count.
func (ti *InferenceContext) matchFuncType(argc int, t types.Type) (args []types.Type, ret types.Type, err error) {
	switch t := t.(type) {
	case *types.Arrow:
		if len(t.Args) != argc {
			return t.Args, t.Return, errors.New("Unexpected number of arguments for applied function")
		}
		return t.Args, t.Return, nil

	case *types.Var:
		switch {
		case t.IsLinkVar():
			return ti.matchFuncType(argc, t.Link())
		case t.IsUnboundVar():
			args = make([]types.Type, argc)
			vars := ti.common.VarTracker.NewList(t.Level(), argc+1)
			tv, tail := vars.Head(), vars.Tail()
			for i := 0; i < argc; i++ {
				args[i] = tv
				tv, tail = tail.Head(), tail.Tail()
			}
			ret = tv
			t.SetLink(&types.Arrow{Args: args, Return: ret})
			return args, ret, nil
		default:
			return nil, nil, errors.New("Type variable for applied function has not been instantiated")
		}
	}

	return nil, nil, errors.New("Unexpected type " + t.TypeName() + " for applied function")
}

// https://github.com/tomprimozic/type-systems/blob/master/extensible_rows2/infer.ml#L287
//
// infer_cases env level return_ty rest_row_ty cases = match cases with
// 	| [] -> rest_row_ty
// 	| (label, var_name, expr) :: other_cases ->
// 			let variant_ty = new_var level in
// 			unify return_ty (infer (Env.extend env var_name variant_ty) level expr) ;
// 			let other_cases_row = infer_cases env level return_ty rest_row_ty other_cases in
// 			TRowExtend(LabelMap.singleton label [variant_ty], other_cases_row)
func (ti *InferenceContext) inferCases(env *TypeEnv, level int, retType, rowType types.Type, cases []ast.MatchCase) (types.Type, error) {
	extensions := make([]types.RowExtend, len(cases))
	vars := ti.common.VarTracker.NewList(level, len(cases))
	tv, tail := vars.Head(), vars.Tail()
	for i := len(cases) - 1; i >= 0; i-- {
		c := cases[i]
		variantType := tv
		stashed := ti.common.Stash(env, c.Var)
		env.Types[c.Var] = variantType
		c.SetVariantType(variantType)
		t, err := ti.infer(env, level, c.Value)
		delete(env.Types, c.Var)
		ti.common.Unstash(env, stashed)
		if err != nil {
			return nil, err
		}
		if err := ti.common.Unify(retType, t); err != nil {
			return nil, err
		}
		labels := types.SingletonTypeMap(c.Label, variantType)
		extensions[i].Row, extensions[i].Labels = rowType, labels
		rowType = &extensions[i]
		tv, tail = tail.Head(), tail.Tail()
	}
	return rowType, nil
}

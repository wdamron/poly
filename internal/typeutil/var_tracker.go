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

package typeutil

import (
	"github.com/wdamron/poly/types"
)

type varList struct {
	head types.Var
	tail *varList
}

// VarList is a linked-list of type-variables allocated by VarTracker.
type VarList struct {
	length int
	list   *varList
}

func (vs VarList) Len() int { return vs.length }

func (vs VarList) Head() *types.Var {
	if vs.length == 0 {
		return nil
	}
	return &vs.list.head
}

func (vs VarList) Tail() VarList {
	if vs.length < 2 {
		return VarList{}
	}
	return VarList{length: vs.length - 1, list: vs.list.tail}
}

// VarTracker allocates type-variables and tracks allocations.
type VarTracker struct {
	NextId uint
	count  int
	head   *varList
	block  []varList
}

func (vt *VarTracker) Reset() { vt.count, vt.head, vt.block = 0, nil, nil }

func (vt *VarTracker) List() VarList { return VarList{length: vt.count, list: vt.head} }

func (vt *VarTracker) FlattenLinks() {
	for nd := vt.head; nd != nil; nd = nd.tail {
		nd.head.Flatten()
	}
}

func (vt *VarTracker) New(level uint) *types.Var {
	if len(vt.block) == 0 {
		vt.block = make([]varList, 8)
	}
	nd := &vt.block[0]
	tv := &nd.head
	vt.block = vt.block[1:]
	tv.SetId(vt.NextId)
	tv.SetLevelNum(level)
	vt.NextId, vt.count = vt.NextId+1, vt.count+1
	nd.tail, vt.head = vt.head, nd
	return tv
}

func (vt *VarTracker) NewList(level uint, count int) VarList {
	for i := 0; i < count; i++ {
		_ = vt.New(level)
	}
	return VarList{length: count, list: vt.head}
}

// +build amd64

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

// gator provides an unsafe region-based memory allocator for Go programs (amd64 only).
package gator

import (
	"errors"
	"unsafe"
)

const (
	RegionBytes       = 1024 * 256
	HeaderBytes       = 1024 * 4
	HeaderMetaBytes   = 64
	HeaderBitmapBytes = 4096 - 64
	RegionMemBytes    = (256 - 4) * 1024

	CellBytes      = 8
	CellCount      = (256 - 4) * 1024 / 8 // 32256
	RegionMetaBits = (256 - 4) * 1024 / 8
)

type RegionFlags uint32

const (
	FlagStaticRegion = 1 << iota
	FlagStackRegion
	FlagDroppedRegion
)

func (f RegionFlags) Static() bool  { return f&FlagStaticRegion != 0 }
func (f RegionFlags) Stack() bool   { return f&FlagStackRegion != 0 }
func (f RegionFlags) Dropped() bool { return f&FlagDroppedRegion != 0 }

type RegionTree struct {
	Root  *Region
	Table map[*Region]struct{}
}

func NewRegionTree() *RegionTree {
	return &RegionTree{Table: make(map[*Region]struct{})}
}

type Region struct {
	Header RegionHeader
	Mem    [CellCount * CellBytes]byte
}

type RegionHeader struct {
	Meta RegionMeta
	// Bits may be used to store a boolean state for each cell in a region.
	Bits [HeaderBitmapBytes / 8]uint64
}

type RegionMeta struct {
	Tree *RegionTree
	// Children form a doubly-linked list.
	Up, Left, Right, Down *Region
	Flags                 RegionFlags
	// Metadata extensions (e.g. stack pointer)
	Ext1 uint32
	Ext2 interface{}
	// Metadata is padded to 64 bytes on 32-bit architectures.
	// 7 additional words will be available for metadata extensions.
	Arch32Ext [regionHeaderMetaPadArch32]uint32
}

const regionHeaderMetaPadArch32 = ((unsafe.Sizeof(0) & 0x20) >> 5) * 7

func (tree *RegionTree) InitRootRegion() (*Region, error) {
	if tree.Root != nil {
		return nil, errors.New("root region already exists")
	}
	r := &Region{
		Header: RegionHeader{Meta: RegionMeta{
			Tree: tree,
		}},
	}
	tree.Root = r
	tree.Table[r] = struct{}{}
	return r, nil
}

func (h *RegionHeader) SetBit(index uint) {
	h.Bits[index/64] |= 1 << (index % 64)
}

func (h *RegionHeader) ClearBit(index uint) {
	h.Bits[index/64] &^= 1 << (index % 64)
}

func (r *Region) NewSubRegion() (*Region, error) {
	rmeta := &r.Header.Meta
	if rmeta.Flags.Dropped() {
		return nil, errors.New("parent region has already been dropped")
	}
	tree := rmeta.Tree
	down := &Region{
		Header: RegionHeader{Meta: RegionMeta{
			Tree:  tree,
			Up:    r,
			Right: rmeta.Down,
			Flags: rmeta.Flags,
		}},
	}
	if rmeta.Down != nil {
		rmeta.Down.Header.Meta.Left = down
	}
	rmeta.Down = down
	tree.Table[down] = struct{}{}

	return down, nil
}

func (r *Region) Drop() error {
	meta := &r.Header.Meta
	if meta.Flags.Dropped() {
		return errors.New("region has already been dropped")
	}
	if meta.Down != nil {
		return errors.New("region cannot be dropped until all sub-regions are dropped")
	}
	tree, up, left, right := meta.Tree, meta.Up, meta.Left, meta.Right
	if left != nil {
		left.Header.Meta.Right = right
	} else {
		up.Header.Meta.Down = right
	}
	if right != nil {
		right.Header.Meta.Left = left
	}
	delete(tree.Table, r)
	if r == tree.Root {
		tree.Root = nil
	}
	return nil
}

func AssignPointer(pointerRegion *Region, pointerMemOffset uint, pointsToRegion *Region, pointsToMemOffset uint) (ok bool) {
	if !CanAssignPointer(pointerRegion, pointsToRegion) {
		return false
	}
	UnsafeAssignPointer(&pointerRegion.Mem[pointerMemOffset], &pointsToRegion.Mem[pointsToMemOffset])
	return true
}

func CanAssignPointer(pointerRegion, pointsToRegion *Region) (ok bool) {
	if pointerRegion.Header.Meta.Tree != pointsToRegion.Header.Meta.Tree {
		return false
	}
	ptrFlags, memFlags := pointerRegion.Header.Meta.Flags, pointsToRegion.Header.Meta.Flags
	if ptrFlags.Dropped() || memFlags.Dropped() {
		return false
	}
	switch (uint64(ptrFlags) << 32) | uint64(memFlags) {
	case (FlagStaticRegion << 32) | FlagStaticRegion, (FlagStackRegion << 32) | FlagStaticRegion:
		return true
	case (FlagStaticRegion << 32) | FlagStackRegion:
		return false
	case (FlagStackRegion << 32) | FlagStackRegion:
		break // check levels
	default:
		switch memFlags {
		case FlagStaticRegion:
			return true
		case FlagStackRegion:
			return false
		}
	}
	if pointerRegion == pointsToRegion {
		return true
	}
	for up := pointerRegion.Header.Meta.Up; up != nil; up = up.Header.Meta.Up {
		if up == pointsToRegion {
			return true
		}
	}
	return false
}

func UnsafeMemRegion(mem *byte) *Region {
	return (*Region)(unsafe.Pointer(uintptr(unsafe.Pointer(mem)) &^ (RegionBytes - 1)))
}

func UnsafeAssignPointer(mem, pointsTo *byte) {
	*((**byte)(unsafe.Pointer(mem))) = pointsTo
}

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
	"github.com/wdamron/poly/internal/typeutil"
	"github.com/wdamron/poly/types"
)

// Generalize all unbound type-variables in t, excluding type-variables within mutable reference-types.
func Generalize(t types.Type) types.Type {
	return typeutil.GeneralizeOpts(types.TopLevel, t, false, false)
}

// Mark all type-variables in t as weakly-polymorphic. Weakly-polymorphic types will not be generalized during inference.
func MarkWeak(t types.Type) types.Type {
	return typeutil.GeneralizeOpts(types.TopLevel, t, false, true)
}

// Generalize all unbound type-variables in t, including type-variables within mutable reference-types.
func GeneralizeRefs(t types.Type) types.Type {
	return typeutil.GeneralizeOpts(types.TopLevel, t, true, false)
}

// Generalize all unbound type-variables in t, excluding type-variables within mutable reference-types.
func GeneralizeAtLevel(level uint, t types.Type) types.Type {
	return typeutil.GeneralizeOpts(level, t, true, false)
}

// Generalize all unbound type-variables in t, marking each generalized variable as weakly-polymorphic.
func GeneralizeWeak(t types.Type) types.Type {
	return typeutil.GeneralizeOpts(types.TopLevel, t, true, true)
}

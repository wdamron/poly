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
	"errors"
)

// Kind represents a qualified type.
type Kind struct {
	// Id must be unique.
	Id   int
	Name string
	// Check or apply kind-constraints for a type. The Kind argument will be a pointer to this kind.
	Refine func(Type, *Kind) error
}

// Create a new union-type qualifier with the given name and unique id, which restricts types
// to the given set of type-constants.
func NewUnion(kindName string, kindId int, anyOf ...*Const) *Kind {
	return &Kind{
		Id:   kindId,
		Name: kindName,
		Refine: func(t Type, k *Kind) error {
			switch link := RealType(t).(type) {
			case *Var:
				t.(*Var).MoveKind(link, k)
				return nil
			case *Const:
				for _, c := range anyOf {
					if link.Name == c.Name {
						return nil
					}
				}
				return errors.New("invalid " + k.Name + " type: " + link.Name)
			case nil:
				return errors.New("invalid " + k.Name + " type: nil")
			}
			return errors.New("invalid " + k.Name + " type: " + t.TypeName())
		},
	}
}

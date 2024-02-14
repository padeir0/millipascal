package util

import (
	. "mpc/pir/errors"
)

func NewInternalSemanticError(debug string) *Error {
	return newInternalError(debug)
}

func newInternalError(message string) *Error {
	e := Error(message)
	return &e
}

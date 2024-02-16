package util

import (
	. "mpc/core"
	et "mpc/core/errorkind"
	ir "mpc/core/module"
	sv "mpc/core/severity"
)

func Place(M *ir.Module, n *ir.Node) *Location {
	return &Location{
		File:  M.FullPath,
		Range: n.Range,
	}
}

type NodeInfo struct {
	N       *ir.Node
	Message string
}

func NewNodeInfo(n *ir.Node, m string) *NodeInfo {
	return &NodeInfo{
		N:       n,
		Message: m,
	}
}

func NewInternalError(M *ir.Module, n *ir.Node, message string) *Error {
	return NewSemanticError(M, et.InternalCompilerError, n, message)
}

func NewInternalSemanticError(debug string) *Error {
	return newInternalError(debug)
}

func newInternalError(message string) *Error {
	return &Error{
		Code:     et.InternalCompilerError,
		Severity: sv.InternalError,
		Message:  message,
	}
}

func NewSemanticError(M *ir.Module, t et.ErrorKind, n *ir.Node, message string) *Error {
	loc := Place(M, n)
	return &Error{
		Code:     t,
		Severity: sv.Error,
		Location: loc,
		Message:  message,
	}
}

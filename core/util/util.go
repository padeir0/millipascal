package util

import (
	. "mpc/core"
	et "mpc/core/errorkind"
	ir "mpc/core/module"
)

func Place(M *ir.Module, n *ir.Node) SourceLocation {
	return SourceLocation{
		File: M.FullPath,
		Line: n.Line,
		Col:  n.Col,
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

func NewInternalSemanticError(debug string) *Error {
	return newInternalError(debug)
}

func newInternalError(debug string) *Error {
	return &Error{
		Type:  et.InternalCompilerError,
		Debug: debug,
		Info:  nil,
	}
}

func NewSemanticError(M *ir.Module, t et.ErrorKind, n ...*NodeInfo) *Error {
	excerpts := []Excerpt{}
	for _, v := range n {
		loc := Place(M, v.N)
		excerpts = append(excerpts, Excerpt{
			Location: &loc,
			Message:  v.Message,
		})
	}
	return &Error{
		Type: t,
		Info: excerpts,
	}
}

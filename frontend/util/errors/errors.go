package errors

import (
	"mpc/frontend/errors"
	et "mpc/frontend/enums/errType"
	"mpc/frontend/ast"
)

func Place(M *ast.Module, n *ast.Node) errors.SourceLocation {
	return errors.SourceLocation{
		File: M.ID,
		Line: n.Line,
		Col:  n.Col,
	}
}

type NodeInfo struct {
	N       *ast.Node
	Message string
}

func NewNodeInfo(n *ast.Node, m string) *NodeInfo {
	return &NodeInfo{
		N: n,
		Message: m,
	}
}

func NewInternalSemanticError(debug string) *errors.CompilerError {
	return newInternalError(errors.Semantic, debug)
}

func newInternalError(stage errors.PipelineStage, debug string) *errors.CompilerError {
	return &errors.CompilerError{
		Stage: stage,
		Type:  et.InternalCompilerError,
		Debug: debug,
		Info:  nil,
	}
}

func NewSemanticError(M *ast.Module, t et.ErrType, n ...*NodeInfo) *errors.CompilerError {
	excerpts := []errors.Excerpt{}
	for _, v := range n {
		loc := Place(M, v.N)
		excerpts = append(excerpts, errors.Excerpt{
			Location: &loc,
			Message:  v.Message,
		})
	}
	return &errors.CompilerError{
		Stage: errors.Semantic,
		Type:  t,
		Info:  excerpts,
	}
}

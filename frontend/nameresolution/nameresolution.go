package nameresolution

import (
	"mpc/frontend/ast"
	"mpc/frontend/errors"
	"mpc/frontend/lexType"
	msg "mpc/frontend/messages"
)

func ResolveNames(M *ast.Module) *errors.CompilerError {
	symbols := M.Root.Leaves[0]
	for _, symbol := range symbols.Leaves {
		err := declareSymbol(M, symbol)
		if err != nil {
			return err
		}
	}
	return nil
}

func declareSymbol(M *ast.Module, n *ast.Node) *errors.CompilerError {
	sy := getSymbol(n)
	v, ok := M.Globals[sy.Name]
	if ok {
		return msg.ErrorNameAlreadyDefined(M, n, v.N)
	}
	M.Globals[sy.Name] = sy
	return nil
}

func getSymbol(n *ast.Node) *ast.Symbol {
	switch n.Lex {
	case T.PROC:
		return &ast.Symbol{
			T:    ast.SProc,
			Name: getSymbolName(n),
			Proc: &ast.Proc{
				Args:  []*ast.Decl{},
				Rets:  []ast.Type{},
				Vars:  []*ast.Decl{},
				Names: map[string]*ast.Decl{},
				Code:  n.Leaves[4],
			},
			N: n,
		}
	case T.MEM:
		return &ast.Symbol{
			T:    ast.SMem,
			Name: getSymbolName(n),
			Mem: &ast.Mem{
				Size: -1,
				Type: ast.InvalidType,
				Init: []*ast.Node{},
			},
			N: n,
		}
	case T.CONST:
		return &ast.Symbol{
			T:    ast.SConst,
			Name: getSymbolName(n),
			N:    n,
		}
	}
	panic("getSymbolType: what")
}

func getSymbolName(n *ast.Node) string {
	return n.Leaves[0].Text
}

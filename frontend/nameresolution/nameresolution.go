package nameresolution

import (
	"mpc/frontend/ast"
	"mpc/frontend/errors"
	lex "mpc/frontend/enums/lexType"
	T "mpc/frontend/enums/Type"
	ST "mpc/frontend/enums/symbolType"
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
	case lex.PROC:
		return &ast.Symbol{
			T:    ST.Proc,
			Name: getSymbolName(n),
			Proc: &ast.Proc{
				Args:  []*ast.Decl{},
				Rets:  []T.Type{},
				Vars:  []*ast.Decl{},
				Names: map[string]*ast.Decl{},
				N:  n.Leaves[4],
			},
			N: n,
		}
	case lex.MEM:
		return &ast.Symbol{
			T:    ST.Mem,
			Name: getSymbolName(n),
			Mem: &ast.Mem{
				Size: -1,
				Type: T.Invalid,
				Init: []*ast.Node{},
			},
			N: n,
		}
	case lex.CONST:
		return &ast.Symbol{
			T:    ST.Const,
			Name: getSymbolName(n),
			N:    n,
		}
	}
	panic("getSymbolType: what")
}

func getSymbolName(n *ast.Node) string {
	return n.Leaves[0].Text
}

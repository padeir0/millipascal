package nameresolution

import (
	T "mpc/frontend/Type"
	lex "mpc/frontend/enums/lexType"
	ST "mpc/frontend/enums/symbolType"
	"mpc/frontend/errors"
	"mpc/frontend/ir"
	msg "mpc/frontend/messages"

	"strconv"
)

func ResolveNames(M *ir.Module) *errors.CompilerError {
	symbols := M.Root.Leaves[0]
	for _, symbol := range symbols.Leaves {
		err := declareSymbol(M, symbol)
		if err != nil {
			return err
		}
	}
	return nil
}

func declareSymbol(M *ir.Module, n *ir.Node) *errors.CompilerError {
	sy := getSymbol(n)
	v, ok := M.Globals[sy.Name]
	if ok {
		return msg.ErrorNameAlreadyDefined(M, n, v.N)
	}
	M.Globals[sy.Name] = sy
	return nil
}

func getSymbol(n *ir.Node) *ir.Symbol {
	name := getSymbolName(n)
	switch n.Lex {
	case lex.PROC:
		return &ir.Symbol{
			T:    ST.Proc,
			Name: name,
			Proc: &ir.Proc{
				Name:   name,
				ArgMap: map[string]ir.PositionalSymbol{},
				Vars:   map[string]*ir.Symbol{},
				N:      n,
			},
			N: n,
		}
	case lex.MEMORY:
		return getMemSymbol(n)
	}
	panic("getSymbolType: what")
}

func getMemSymbol(n *ir.Node) *ir.Symbol {
	size, err := strconv.Atoi(n.Leaves[1].Text)
	if err != nil {
		panic("getMemSymbol: " + err.Error())
	}
	mem := &ir.Mem{
		Size: size,
		Init: []*ir.Node{},
	}
	return &ir.Symbol{
		T:    ST.Mem,
		Type: T.T_Ptr,
		Name: getSymbolName(n),
		Mem:  mem,
		N:    n,
	}
}

func getSymbolName(n *ir.Node) string {
	return n.Leaves[0].Text
}

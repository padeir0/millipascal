package nameresolution

import (
	. "mpc/core"
	ir "mpc/core/module"
	lex "mpc/core/module/lexkind"
	ST "mpc/core/module/symbolkind"
	T "mpc/core/types"
	msg "mpc/frontend/messages"
)

func ResolveNames(M *ir.Module) *Error {
	err := resolve(M)
	if err != nil {
		return err
	}
	M.ResetVisited()
	return nil
}

func resolve(M *ir.Module) *Error {
	if M.Visited {
		return nil
	}
	M.Visited = true
	for _, dep := range M.Dependencies {
		err := resolve(dep.M)
		if err != nil {
			return err
		}
	}

	err := createImportedSymbols(M)
	if err != nil {
		return err
	}

	err = createGlobals(M)
	if err != nil {
		return err
	}

	err = checkExports(M)
	if err != nil {
		return err
	}

	return nil
}

func createGlobals(M *ir.Module) *Error {
	symbols := M.Root.Leaves[1]
	for _, symbol := range symbols.Leaves {
		err := declareSymbol(M, symbol)
		if err != nil {
			return err
		}
	}
	return nil
}

func importSymbols(M *ir.Module, n *ir.Node) *Error {
	for _, m := range n.Leaves {
		err := defineModSymbol(M, m)
		if err != nil {
			return err
		}
	}
	return nil
}

func defineModSymbol(M *ir.Module, n *ir.Node) *Error {
	name := n.Text
	sy := &ir.Symbol{
		Name:       name,
		T:          ST.Module,
		ModuleName: M.Name,
		N:          n,
	}
	v, ok := M.Globals[name]
	if ok {
		return msg.ErrorNameAlreadyDefined(M, n, v.N)
	}
	M.Globals[name] = sy
	return nil
}

func createImportedSymbols(M *ir.Module) *Error {
	coupling := M.Root.Leaves[0]
	for _, n := range coupling.Leaves {
		switch n.Lex {
		case lex.IMPORT:
			err := importSymbols(M, n)
			if err != nil {
				return err
			}
		case lex.FROM:
			err := fromImportSymbols(M, n)
			if err != nil {
				return err
			}
		}
	}
	return nil
}

func fromImportSymbols(M *ir.Module, n *ir.Node) *Error {
	mod := n.Leaves[0].Text
	dep, ok := M.Dependencies[mod]
	if !ok {
		panic("dependency should have been found")
	}
	ids := n.Leaves[1]
	for _, id := range ids.Leaves {
		sy, ok := dep.M.Exported[id.Text]
		if !ok {
			return msg.NameNotExported(M, id)
		}
		err := defineExternalSymbol(M, id, sy)
		if err != nil {
			return err
		}
	}
	return nil
}

func defineExternalSymbol(M *ir.Module, n *ir.Node, sy *ir.Symbol) *Error {
	newSy := &ir.Symbol{
		Name:       sy.Name,
		T:          sy.T,
		N:          sy.N,
		ModuleName: sy.ModuleName,
		External:   true,
	}
	v, ok := M.Globals[sy.Name]
	if ok {
		return msg.ErrorNameAlreadyDefined(M, n, v.N)
	}
	M.Globals[sy.Name] = newSy
	return nil
}

func checkExports(M *ir.Module) *Error {
	// check if there are duplicated exports
	// look at global symbols and check if they exist
	// insert into the export list
	coupling := M.Root.Leaves[0]
	exported := map[string]*ir.Node{}
	for _, exp := range coupling.Leaves {
		if exp.Lex == lex.EXPORT {
			for _, name := range exp.Leaves {
				n, ok := exported[name.Text]
				if ok {
					return msg.ErrorDuplicatedExport(M, n, name)
				}
				exported[name.Text] = name
			}
		}
	}
	for name, n := range exported {
		sy, ok := M.Globals[name]
		if !ok {
			return msg.ErrorExportingUndefName(M, n)
		}
		M.Exported[name] = sy
	}
	return nil
}

func declareSymbol(M *ir.Module, n *ir.Node) *Error {
	sy := getSymbol(M, n)
	v, ok := M.Globals[sy.Name]
	if ok {
		return msg.ErrorNameAlreadyDefined(M, n, v.N)
	}
	M.Globals[sy.Name] = sy
	return nil
}

func getSymbol(M *ir.Module, n *ir.Node) *ir.Symbol {
	name := getSymbolName(n)
	switch n.Lex {
	case lex.PROC:
		return &ir.Symbol{
			T:          ST.Proc,
			Name:       name,
			ModuleName: M.Name,
			Proc: &ir.Proc{
				Name:   name,
				ArgMap: map[string]ir.PositionalSymbol{},
				Vars:   map[string]ir.PositionalSymbol{},
				N:      n,
			},
			N: n,
		}
	case lex.MEMORY:
		return getMemSymbol(M, n)
	}
	panic("getSymbolType: what")
}

func getMemSymbol(M *ir.Module, n *ir.Node) *ir.Symbol {
	name := getSymbolName(n)
	mem := &ir.Mem{
		Name: name,
		Init: n.Leaves[1],
	}
	return &ir.Symbol{
		T:          ST.Mem,
		Type:       T.T_Ptr,
		Name:       name,
		ModuleName: M.Name,
		Mem:        mem,
		N:          n,
	}
}

func getSymbolName(n *ir.Node) string {
	return n.Leaves[0].Text
}

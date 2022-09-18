package backend

import (
	"mpc/frontend/ast"
	"mpc/backend/regalloc"
	"mpc/backend/codegen"
)

func Generate(M *ast.Module) string {
	regalloc.Allocate(M, 2)// len(codegen.X64Registers))
	return codegen.Generate(M)
}

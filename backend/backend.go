package backend

import (
	"mpc/frontend/ir"
	"mpc/backend/regalloc"
)

func Generate(M *ir.Module) string {
	regalloc.Allocate(M, 2)// len(codegen.X64Registers))
	return ""
}

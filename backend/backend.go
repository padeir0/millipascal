package backend

import (
	"mpc/frontend/errors"
	"mpc/frontend/ir"
	"mpc/backend/regalloc"
	"mpc/backend/mirchecker"
)

func Generate(M *ir.Module) *errors.CompilerError {
	regalloc.Allocate(M, 2)// len(codegen.X64Registers))
	return mirchecker.Check(M)
}

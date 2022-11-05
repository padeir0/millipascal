package backend

import (
	"mpc/frontend/errors"
	"mpc/frontend/ir"
	"mpc/backend/regalloc"
	"mpc/backend/mirchecker"
)

// TODO: Write_Stdout and Read_Stdin procedures
func Generate(M *ir.Module) *errors.CompilerError {
	regalloc.Allocate(M, 2)
	return mirchecker.Check(M)
}

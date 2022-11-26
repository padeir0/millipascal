package backend

import (
	"mpc/frontend/errors"
	"mpc/frontend/ir"
	"mpc/backend/resalloc"
	"mpc/backend/mirchecker"
)

// TODO: Write_Stdout and Read_Stdin procedures
func Generate(M *ir.Module) *errors.CompilerError {
	resalloc.Allocate(M, 2)
	return mirchecker.Check(M)
}

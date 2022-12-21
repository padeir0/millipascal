package backend

import (
	"mpc/frontend/errors"
	"mpc/frontend/ir"
	"mpc/backend/resalloc"
	"mpc/backend/mirchecker"
	"mpc/backend/amd64"
)

func Generate(M *ir.Module) (string, *errors.CompilerError) {
	resalloc.Allocate(M, len(amd64.Registers))
	err := mirchecker.Check(M)
	if err != nil {
		return "", err
	}
	out := amd64.Generate(M)
	return out.Fasmify(), nil
}

func Mir(M *ir.Module) *errors.CompilerError {
	resalloc.Allocate(M, len(amd64.Registers))
	err := mirchecker.Check(M)
	if err != nil {
		return err
	}
	return nil
}

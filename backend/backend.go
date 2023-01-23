package backend

import (
	"fmt"
	"mpc/backend/amd64"
	"mpc/backend/resalloc"
	. "mpc/core"
	"mpc/core/hir"
	"mpc/core/mir"
	mirchecker "mpc/core/mir/checker"
)

var NumRegisters = len(amd64.Registers)

func Generate(P *hir.Program) (string, *Error) {
	mirP := resalloc.Allocate(P, NumRegisters)
	err := mirchecker.Check(mirP)
	if err != nil {
		return "", err
	}
	out := amd64.Generate(mirP)
	return out.String(), nil
}

func Mir(M *hir.Program) (*mir.Program, *Error) {
	mirP := resalloc.Allocate(M, NumRegisters)
	fmt.Println(mirP)
	err := mirchecker.Check(mirP)
	if err != nil {
		return nil, err
	}
	return mirP, nil
}

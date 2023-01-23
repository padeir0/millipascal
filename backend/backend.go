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

func Generate(P *hir.Program) (string, *Error) {
	mirP := resalloc.Allocate(P, len(amd64.Registers))
	err := mirchecker.Check(mirP)
	if err != nil {
		return "", err
	}
	out := amd64.Generate(mirP)
	return out.String(), nil
}

func Mir(M *hir.Program) (*mir.Program, *Error) {
	mirP := resalloc.Allocate(M, len(amd64.Registers))
	fmt.Println(mirP)
	err := mirchecker.Check(mirP)
	if err != nil {
		return nil, err
	}
	return mirP, nil
}

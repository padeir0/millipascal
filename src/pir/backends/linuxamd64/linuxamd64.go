package linuxamd64

import (
	"mpc/pir"
	"mpc/pir/backends/linuxamd64/fasm"
	"mpc/pir/backends/linuxamd64/resalloc"
	. "mpc/pir/errors"

	mirchecker "mpc/pir/backends/linuxamd64/mir/checker"
	pirchecker "mpc/pir/checker"
)

func GenerateFasm(p *pir.Program, outname string) (string, *Error) {
	err := pirchecker.Check(p)
	if err != nil {
		return "", err
	}
	mirProgram := resalloc.Allocate(p, len(fasm.Registers))
	err = mirchecker.Check(mirProgram)
	if err != nil {
		return "", err
	}
	fasmProgram := fasm.Generate(mirProgram, outname)
	return fasmProgram.Contents, nil
}

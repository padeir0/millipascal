package main

import (
	"mpc/frontend"
	"mpc/frontend/ir"
	"mpc/backend"
	"mpc/testing"
	. "mpc/util"
	"flag"
	"os"
	"strconv"
)

var lexOnly = flag.Bool("lex", false, "runs the lexer and prints the tokens")
var parseOnly = flag.Bool("parse", false, "runs the lexer and parser, prints AST output")
var hirOnly = flag.Bool("hir", false, "runs the full frontend, prints hir")
var mirOnly = flag.Bool("mir", false, "runs the full compiler, prints mir")
var asmOnly = flag.Bool("asm", false, "runs the full compiler, prints asm")

var test = flag.Bool("test", false, "runs tests for all files in a folder,"+
	" you can specify the stage to test using the other flags\n"+
	"\t ex: anuc -lex   -test folder/\n"+
	"\t     anuc -parse -test folder/")

func main() {
	flag.Parse()
	args := flag.Args()
	if len(args) != 1 {
		Fatal("invalid number of arguments\n")
	}
	eval(args[0])
}

func eval(s string) {
	checkValid()
	if *test {
		res := Test(s, testingMode())
		printResults(res)
		return
	}
	normalMode(s)
}

func testingMode() testing.Tester {
	switch true {
	case *lexOnly:
		return testing.Lex
	case *parseOnly:
		return testing.Parse
	case *hirOnly:
		return testing.Hir
	case *mirOnly:
		return testing.Mir
	case *asmOnly:
		return testing.Mir
	default:
		return testing.All
	}
}

func normalMode(s string) {
	switch true {
	case *lexOnly:
		lexemes, err := frontend.Lex(s)
		OkOrBurst(err)
		for _, lexeme := range lexemes {
			Stdout(lexeme.String())
			Stdout("\n")
		}
	case *parseOnly:
		n, err := frontend.Parse(s)
		Stdout("\n-----AST\n")
		Stdout(ir.FmtNode(n))
		Stdout("\n")
		OkOrBurst(err)
	case *hirOnly:
		m, err := frontend.All(s)
		Stdout("\n-----AST\n")
		Stdout(m.String())
		Stdout("\n-----HIR\n")
		Stdout(m.StringifyCode())
		Stdout("\n")
		OkOrBurst(err)

	case *mirOnly:
		m, err := frontend.All(s)
		Stdout("\n-----AST\n")
		Stdout(m.String())
		Stdout("\n-----HIR\n")
		Stdout(m.StringifyCode())
		OkOrBurst(err)

		err = backend.Mir(m)
		Stdout("\n-----MIR\n")
		Stdout(m.StringifyCode())
		OkOrBurst(err)
	case *asmOnly:
		m, err := frontend.All(s)
		OkOrBurst(err)
		s, err = backend.Generate(m)
		OkOrBurst(err)
		Stdout(s)
	default:
		m, err := frontend.All(s)
		OkOrBurst(err)
		s, err = backend.Generate(m)
		OkOrBurst(err)
		e := Fasm(s, m.Name)
		if e != nil {
			Fatal(e.Error())
		}
	}
}

func checkValid() {
	var selected = []bool{*lexOnly, *parseOnly, *hirOnly, *mirOnly, *asmOnly}
	var count = 0
	for _, b := range selected {
		if b {
			count++
		}
	}
	if count > 1 {
		Fatal("only one of lex, parse, hir, mir or asm flags may be used at a time")
	}
}

func printResults(results []*testing.TestResult) {
	failed := 0
	Stdout("\n")
	for _, res := range results {
		if !res.Ok && res.Message != "" {
			Stdout(res.File + "\t" + res.Message + "\n")
		}
		if !res.Ok {
			failed += 1
		}
	}
	Stdout("\n")
	Stdout("failed: " + strconv.Itoa(failed) + "\n")
	Stdout("total: " + strconv.Itoa(len(results)) + "\n")
}

func Test(folder string, tester testing.Tester) []*testing.TestResult {
	entries, err := os.ReadDir(folder)
	if err != nil {
		Fatal(err.Error() + "\n")
	}
	results := []*testing.TestResult{}
	for _, v := range entries {
		fullpath := folder + "/" + v.Name()
		Stdout("testing: " + fullpath + "\t")
		if v.IsDir() {
			res := Test(fullpath, tester)
			results = append(results, res...)
			Stdout("\n")
		} else {
			res := tester(fullpath)
			results = append(results, &res)
			Stdout(res.String() + "\n")
		}
	}
	return results
}

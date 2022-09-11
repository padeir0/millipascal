package main

import (
	"mpc/frontend"
	"mpc/frontend/ast"
	"mpc/testing"
	. "mpc/util"
	"flag"
	"os"
	"strconv"
)

var lexOnly = flag.Bool("lex", false, "runs the lexer and prints the tokens")
var parseOnly = flag.Bool("parse", false, "runs the lexer and parser, prints AST output")
var frontendOnly = flag.Bool("frontend", false, "runs the full frontend, prints the fully typed module")

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
	case *frontendOnly:
		return testing.Frontend
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
		OkOrBurst(err)
		Stdout(ast.FmtNode(n))
		Stdout("\n")
	case *frontendOnly:
		m, err := frontend.All(s)
		OkOrBurst(err)
		Stdout(m.String())
		Stdout("\n")
	default:
		m, err := frontend.All(s)
		OkOrBurst(err)
		Stdout(m.String())
		Stdout("\n")
	}
}

func checkValid() {
	var selected = []bool{*lexOnly, *parseOnly, *frontendOnly}
	var count = 0
	for _, b := range selected {
		if b {
			count++
		}
	}
	if count > 1 {
		Fatal("only one of lex, parse, resolve, typecheck, gen or vm flags may be used at a time")
	}
}

func printResults(results []*testing.TestResult) {
	failed := []*testing.TestResult{}
	for _, res := range results {
		Stdout(res.String() + "\t" + res.File + "\n")
		if !res.Ok {
			failed = append(failed, res)
		}
	}
	Stdout("\n\n")
	for _, res := range failed {
		Stdout(res.File + "\t" + res.Message + "\n")
	}
	Stdout("failed: " + strconv.Itoa(len(failed)) + "\n")
	Stdout("total: " + strconv.Itoa(len(results)) + "\n")
}

func Test(folder string, tester testing.Tester) []*testing.TestResult {
	entries, err := os.ReadDir(folder)
	if err != nil {
		Stdout("here")
		Fatal(err.Error())
	}
	results := []*testing.TestResult{}
	for _, v := range entries {
		fullpath := folder + "/" + v.Name()
		if v.IsDir() {
			res := Test(fullpath, tester)
			results = append(results, res...)
		} else {
			res := tester(fullpath)
			results = append(results, &res)
		}
	}
	return results
}

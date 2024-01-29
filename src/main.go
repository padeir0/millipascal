package main

import (
	"flag"
	"fmt"
	. "mpc/core"
	"mpc/format"
	"mpc/pipelines"
	"mpc/testing"
	"os"
	"strconv"
	"strings"
	"time"

	"runtime/pprof"
)

var lexemes = flag.Bool("lexemes", false, "runs the lexer and prints the tokens")
var ast = flag.Bool("ast", false, "runs the lexer and parser, prints AST output")
var mod = flag.Bool("mod", false, "runs the full frontend, prints typed AST")
var pir = flag.Bool("pir", false, "runs the full frontend, prints pir")
var mir = flag.Bool("mir", false, "runs the full compiler, prints mir")
var asm = flag.Bool("asm", false, "runs the full compiler, prints asm")
var _format = flag.Bool("fmt", false, "formats code and prints it to stdout")

var test = flag.Bool("test", false, "runs tests for all files in a folder")
var testTimeout = flag.Duration("testtimeout", 5*time.Second, "sets timeout limit for a test")

var verbose = flag.Bool("v", false, "verbose tests")
var outname = flag.String("o", "", "output name of file")

var profile = flag.Bool("prof", false, "start profiler")

func main() {
	flag.Parse()
	if *profile {
		file := "out.pprof"
		f, err := os.Create(file)
		if err != nil {
			fmt.Println(err)
			os.Exit(0)
		}
		pprof.StartCPUProfile(f)
		defer pprof.StopCPUProfile()
	}
	args := flag.Args()
	if len(args) != 1 {
		Fatal("invalid number of arguments\n")
	}
	eval(args[0])
}

func eval(filename string) {
	checkValid()
	if !strings.Contains(filename, "/") {
		filename = "./" + filename
	}
	if *test {
		var res []*testing.TestResult
		res = Test(filename, getStage(), *testTimeout)
		printResults(res)
		return
	}
	normalMode(filename)
}

func normalMode(filename string) {
	switch true {
	case *lexemes:
		lexemes, err := pipelines.Lexemes(filename)
		OkOrBurst(err)
		output := []string{}
		for _, lexeme := range lexemes {
			output = append(output, lexeme.Text)
		}
		fmt.Println(strings.Join(output, ", "))
	case *ast:
		n, err := pipelines.Ast(filename)
		OkOrBurst(err)
		fmt.Println(n)
	case *mod:
		m, err := pipelines.Mod(filename)
		OkOrBurst(err)
		fmt.Println(m)
	case *pir:
		pirP, err := pipelines.Pir(filename)
		OkOrBurst(err)
		fmt.Println(pirP)
	case *mir:
		mirP, err := pipelines.Mir(filename)
		OkOrBurst(err)
		fmt.Println(mirP)
	case *asm:
		fp, err := pipelines.Fasm(filename, *outname)
		OkOrBurst(err)
		fmt.Println(fp.Contents)
	case *_format:
		n, err := pipelines.Ast(filename)
		OkOrBurst(err)
		fmt.Println(format.Format(n))
	default:
		_, err := pipelines.Compile(filename, *outname)
		OkOrBurst(err)
	}
}

func checkValid() {
	var selected = []bool{*lexemes, *ast, *mod, *pir, *mir, *asm, *_format}
	var count = 0
	for _, b := range selected {
		if b {
			count++
		}
	}
	if count > 1 {
		Fatal("only one of lex, parse, mod, pir, mir, asm or fmt flags may be used at a time")
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

func Test(folder string, st testing.Stage, t time.Duration) []*testing.TestResult {
	entries, err := os.ReadDir(folder)
	if err != nil {
		Fatal(err.Error() + "\n")
	}
	results := []*testing.TestResult{}
	for _, v := range entries {
		fullpath := folder + "/" + v.Name()
		if v.IsDir() {
			if *verbose {
				Stdout("\u001b[35m entering: " + fullpath + "\u001b[0m\n")
			}
			res := Test(fullpath, st, t)
			results = append(results, res...)
			if *verbose {
				Stdout("\u001b[35m leaving: " + fullpath + "\u001b[0m\n")
			}
		} else if strings.HasSuffix(v.Name(), ".mp") {
			res := testing.Test(fullpath, st, t)
			results = append(results, &res)
			if *verbose {
				Stdout("testing: " + fullpath + "\t")
				Stdout(res.String() + "\n")
			}
		}
	}
	return results
}

func getStage() testing.Stage {
	switch {
	case *lexemes:
		return testing.S_Lexer
	case *ast:
		return testing.S_Parser
	case *mod:
		return testing.S_Typechecker
	case *pir:
		return testing.S_PirGeneration
	case *mir:
		return testing.S_MirGeneration
	case *asm:
		return testing.S_FasmGeneration
	case *_format:
		return testing.S_Format
	default:
		return testing.S_Compile
	}
}

func OkOrBurst(e *Error) {
	if e != nil {
		Fatal(e.String() + "\n")
	}
}

func Stdout(s string) {
	os.Stdout.Write([]byte(s))
}

func Fatal(s string) {
	os.Stderr.Write([]byte(s))
	os.Exit(0)
}

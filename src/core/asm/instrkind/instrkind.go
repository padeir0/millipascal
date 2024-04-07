package instrkind

type InstrKind int

const (
	InvalidInstrKind InstrKind = iota
	Mov
	Movsx
	Movzx
	Movsxd
	Xor
	Or
	And
	Not
	Shl
	Shr
	Sal
	Sar
	Cmp
	Syscall
	Call
	Ret
	Push
	Pop
	Jmp
	Je
	Jne
	Jl
	Jle
	Jg
	Jge
	Jb
	Jbe
	Ja
	Jae
	Add
	Sub
	Neg
	IDiv
	Div
	Mul
	IMul
	Sete
	Setne
	Setg
	Setge
	Setl
	Setle
	Seta
	Setae
	Setb
	Setbe
	Nop
	Cdq
	Cqo
)

func StringToKind(s string) InstrKind {
	switch s {
	case "nop":
		return Nop
	case "cdq":
		return Cdq
	case "cqo":
		return Cqo
	case "mov":
		return Mov
	case "movsx":
		return Movsx
	case "movzx":
		return Movzx
	case "movsxd":
		return Movsxd
	case "xor":
		return Xor
	case "or":
		return Or
	case "and":
		return And
	case "not":
		return Not
	case "shl":
		return Shl
	case "shr":
		return Shr
	case "sal":
		return Sal
	case "sar":
		return Sar
	case "cmp":
		return Cmp
	case "syscall":
		return Syscall
	case "call":
		return Call
	case "ret":
		return Ret
	case "push":
		return Push
	case "pop":
		return Pop
	case "jmp":
		return Jmp
	case "je":
		return Je
	case "jne":
		return Jne
	case "jl":
		return Jl
	case "jle":
		return Jle
	case "jg":
		return Jg
	case "jge":
		return Jge
	case "jb":
		return Jb
	case "jbe":
		return Jbe
	case "ja":
		return Ja
	case "jae":
		return Jae
	case "add":
		return Add
	case "sub":
		return Sub
	case "neg":
		return Neg
	case "idiv":
		return IDiv
	case "div":
		return Div
	case "mul":
		return Mul
	case "imul":
		return IMul
	case "sete":
		return Sete
	case "setne":
		return Setne
	case "setg":
		return Setg
	case "setge":
		return Setge
	case "setl":
		return Setl
	case "setle":
		return Setle
	case "seta":
		return Seta
	case "setae":
		return Setae
	case "setb":
		return Setb
	case "setbe":
		return Setbe
	default:
		return InvalidInstrKind
	}
}

func KindToString(k InstrKind) string {
	switch k {
	case Nop:
		return "nop"
	case Cdq:
		return "cdq"
	case Cqo:
		return "cqo"
	case Mov:
		return "mov"
	case Movsx:
		return "movsx"
	case Movzx:
		return "movzx"
	case Movsxd:
		return "movsxd"
	case Xor:
		return "xor"
	case Or:
		return "or"
	case And:
		return "and"
	case Not:
		return "not"
	case Shl:
		return "shl"
	case Shr:
		return "shr"
	case Sal:
		return "sal"
	case Sar:
		return "sar"
	case Cmp:
		return "cmp"
	case Syscall:
		return "syscall"
	case Call:
		return "call"
	case Ret:
		return "ret"
	case Push:
		return "push"
	case Pop:
		return "pop"
	case Jmp:
		return "jmp"
	case Je:
		return "je"
	case Jne:
		return "jne"
	case Jl:
		return "jl"
	case Jle:
		return "jle"
	case Jg:
		return "jg"
	case Jge:
		return "jge"
	case Jb:
		return "jb"
	case Jbe:
		return "jbe"
	case Ja:
		return "ja"
	case Jae:
		return "jae"
	case Add:
		return "add"
	case Sub:
		return "sub"
	case Neg:
		return "neg"
	case IDiv:
		return "idiv"
	case Div:
		return "div"
	case Mul:
		return "mul"
	case IMul:
		return "imul"
	case Sete:
		return "sete"
	case Setne:
		return "setne"
	case Setg:
		return "setg"
	case Setge:
		return "setge"
	case Setl:
		return "setl"
	case Setle:
		return "setle"
	case Seta:
		return "seta"
	case Setae:
		return "setae"
	case Setb:
		return "setb"
	case Setbe:
		return "setbe"
	default:
		return "??"
	}
}

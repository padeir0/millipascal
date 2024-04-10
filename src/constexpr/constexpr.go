package constexpr

import (
	"math/big"

	. "mpc/core"
	"mpc/core/asm"
	au "mpc/core/asm/util"
	mod "mpc/core/module"
	gk "mpc/core/module/globalkind"
	lk "mpc/core/module/lexkind"
	T "mpc/core/types"
	util "mpc/core/util"
	msg "mpc/messages"
)

var i8_min = big.NewInt(-(1 << 7))
var i8_max = big.NewInt((1 << 7) - 1)

var i16_min = big.NewInt(-(1 << 15))
var i16_max = big.NewInt((1 << 15) - 1)

var i32_min = big.NewInt(-(1 << 31))
var i32_max = big.NewInt((1 << 31) - 1)

var i64_min = big.NewInt(-(1 << 63))
var i64_max = big.NewInt((1 << 63) - 1)

var zero = big.NewInt(0)
var one = big.NewInt(1)

var u8_max = big.NewInt((1 << 8) - 1)
var u16_max = big.NewInt((1 << 16) - 1)
var u32_max = big.NewInt((1 << 32) - 1)
var u64_max = new(big.Int).SetUint64((1 << 64) - 1)

func getMinMax(t *T.Type) (*big.Int, *big.Int) {
	switch t.Basic {
	case T.I8:
		return i8_min, i8_max
	case T.I16:
		return i16_min, i16_max
	case T.I32:
		return i32_min, i32_max
	case T.I64:
		return i64_min, i64_max

	case T.U8:
		return zero, u8_max
	case T.U16:
		return zero, u16_max
	case T.U32:
		return zero, u32_max
	case T.U64:
		return zero, u64_max
	case T.Ptr:
		return zero, u64_max
	case T.Bool:
		return zero, one
	default:
		panic("should be unreachable")
	}
}

func EvalConstExprs(m *mod.Module) *Error {
	m.ResetVisited()
	err := evalModule(m)
	if err != nil {
		return err
	}
	m.ResetVisited()
	return nil
}

func evalModule(m *mod.Module) *Error {
	if m.Visited {
		return nil
	}
	m.Visited = true
	var err *Error
	for _, dep := range m.Dependencies {
		err = evalModule(dep.M)
		if err != nil {
			return err
		}
	}

	m.ResetVisitedSymbols()
	for _, sy := range m.Globals {
		if !sy.External {
			err = evalSymbol(m, mod.FromSymbol(sy))
			if err != nil {
				return err
			}
		}
	}
	return nil
}

func evalSymbol(m *mod.Module, sf mod.SyField) *Error {
	if sf.IsVisited() {
		return nil
	}
	sf.SetVisited(true)
	err := evalDeps(m, sf)
	if err != nil {
		return err
	}

	if sf.IsField() {
		f := sf.GetField()
		num, err := Compute(m, f.Offset)
		f.Offset.Value = num
		if err != nil {
			return err
		}
	}

	switch sf.Sy.Kind {
	case gk.Const:
		err = evalConst(m, sf.Sy)
	case gk.Data:
		err = evalData(m, sf.Sy)
	case gk.Struct:
		err = evalStruct(m, sf.Sy)
	case gk.Proc:
		err = evalProc(m, sf.Sy)
	}
	if err != nil {
		return err
	}
	return nil
}

func evalDeps(m *mod.Module, sy mod.SyField) *Error {
	return evalRefs(m, sy.GetRefs())
}

func evalRefs(m *mod.Module, refs mod.Refs) *Error {
	for _, ref := range refs.Symbols {
		err := evalSymbol(m, ref)
		if err != nil {
			return err
		}
	}
	return nil
}

func evalStruct(m *mod.Module, sy *mod.Global) *Error {
	t := sy.Struct.Type
	if t.Struct.WellBehaved {
		size := 0
		for i, field := range t.Struct.Fields {
			t.Struct.Fields[i].Offset = big.NewInt(int64(size))
			size += field.Type.Size()
		}
		t.Struct.Size = big.NewInt(int64(size))
	} else {
		size := sy.N.Leaves[1]
		value, err := computeExpr(m, size)
		if err != nil {
			return err
		}
		t.Struct.Size = value
		for i, field := range sy.Struct.Fields {
			if !field.Visited {
				field.Visited = true
				err := evalRefs(m, field.Refs)
				if err != nil {
					return err
				}
				offset, err := computeExpr(m, field.Offset)
				if err != nil {
					return err
				}
				t.Struct.Fields[i].Offset = offset
			}
		}
	}
	return nil
}

func evalConst(m *mod.Module, sy *mod.Global) *Error {
	v, err := Compute(m, sy.N.Leaves[2])
	if err != nil {
		return err
	}
	sy.Const.Value = v
	return nil
}

func evalData(M *mod.Module, sy *mod.Global) *Error {
	arg := sy.N.Leaves[2]
	if arg == nil {
		sy.Data.Size = sy.Data.Type.Sizeof()
		return nil
	}
	switch arg.Lex {
	case lk.STRING_LIT:
		size := stringSize(arg.Text)
		sy.Data.Size = big.NewInt(int64(size))
		sy.Data.Contents = arg.Text
		return nil
	case lk.BLOB:
		return evalBlob(M, sy, arg)
	default:
		v, err := Compute(M, arg)
		if err != nil {
			return err
		}
		if T.IsStruct(sy.Data.Type) {
			scratch := big.NewInt(0)
			size := sy.Data.DataTypeSize(M)
			sy.Data.Size = scratch.Mul(v, size)
		} else {
			sy.Data.Size = v
		}
		return nil
	}
}

func evalBlob(m *mod.Module, sy *mod.Global, blob *mod.Node) *Error {
	nums := make([]asm.DataEntry, len(blob.Leaves))
	size := 0
	for i, leaf := range blob.Leaves {
		num, err := Compute(m, leaf)
		if err != nil {
			return err
		}
		nums[i] = asm.DataEntry{
			Num:  num,
			Type: au.TypeToTsize(leaf.Type),
		}
		size += leaf.Type.Size()
	}
	sy.Data.Nums = nums
	sy.Data.Size = big.NewInt(int64(size))
	return nil
}

func evalProc(M *mod.Module, sy *mod.Global) *Error {
	body := sy.N.Leaves[4]
	// this code is just trying to find each
	// const expression in the asm code :)
	if body.Lex == lk.ASM {
		lines := body.Leaves[0]
		for _, line := range lines.Leaves {
			if line.Lex == lk.INSTR {
				opList := line.Leaves[1]
				err := evalOpList(M, opList)
				if err != nil {
					return err
				}
			}
		}
	}
	return nil
}

func evalOpList(M *mod.Module, opList *mod.Node) *Error {
	for _, op := range opList.Leaves {
		if op.Lex == lk.LEFTBRACE {
			expr := op.Leaves[0]
			num, err := computeExpr(M, expr)
			if err != nil {
				return err
			}
			op.Value = num
		}
		if op.Lex == lk.LEFTBRACKET {
			innerList := op.Leaves[0]
			err := evalOpList(M, innerList)
			if err != nil {
				return err
			}
		}
	}
	return nil
}

func Compute(m *mod.Module, n *mod.Node) (*big.Int, *Error) {
	min, max := getMinMax(n.Type)
	res, err := computeExpr(m, n)
	if err != nil {
		return nil, err
	}
	if res == nil {
		return nil, badConstExpr(m, n)
	}
	if res.Cmp(min) == -1 || res.Cmp(max) == 1 {
		return nil, msg.ValueOutOfBounds(m, n, res)
	}
	return res, nil
}

// these copious amounts of allocation should not matter
// for small programs, but i can refactor later :)
// nodes keep a max of 2 integers as scratch space
// and 1 as result, this should be a low hanging fruit
func computeExpr(m *mod.Module, n *mod.Node) (*big.Int, *Error) {
	switch n.Lex {
	case lk.STRING_LIT, lk.CALL, lk.AT:
		panic("invalid")
	case lk.IDENTIFIER:
		return getIDValue(m, n), nil
	case lk.DOT:
		return getDotAccess(m, n)
	case lk.DOUBLECOLON:
		return getExternalSymbol(m, n), nil
	case lk.SIZEOF:
		return getSizeof(m, n)
	case lk.I64_LIT, lk.I32_LIT, lk.I16_LIT, lk.I8_LIT,
		lk.U64_LIT, lk.U32_LIT, lk.U16_LIT, lk.U8_LIT,
		lk.FALSE, lk.TRUE, lk.PTR_LIT,
		lk.CHAR_LIT:
		return n.Value, nil
	case lk.PLUS, lk.MINUS, lk.MULTIPLICATION, lk.DIVISION,
		lk.REMAINDER, lk.BITWISEAND, lk.BITWISEXOR, lk.BITWISEOR,
		lk.SHIFTLEFT, lk.SHIFTRIGHT, lk.EQUALS, lk.DIFFERENT,
		lk.MORE, lk.MOREEQ, lk.LESS, lk.LESSEQ:
		left, err := computeExpr(m, n.Leaves[0])
		if err != nil {
			return nil, err
		}
		right, err := computeExpr(m, n.Leaves[1])
		if err != nil {
			return nil, err
		}
		switch n.Lex {
		case lk.PLUS:
			return big.NewInt(0).Add(left, right), nil
		case lk.MINUS:
			return big.NewInt(0).Sub(left, right), nil
		case lk.MULTIPLICATION:
			return big.NewInt(0).Mul(left, right), nil
		case lk.DIVISION:
			return big.NewInt(0).Div(left, right), nil
		case lk.REMAINDER:
			return big.NewInt(0).Rem(left, right), nil
		case lk.BITWISEAND:
			return big.NewInt(0).And(left, right), nil
		case lk.BITWISEXOR:
			return big.NewInt(0).Xor(left, right), nil
		case lk.BITWISEOR:
			return big.NewInt(0).Or(left, right), nil
		case lk.SHIFTLEFT:
			if !right.IsUint64() {
				return nil, msg.ValueOutOfBounds(m, n, right)
			}
			return big.NewInt(0).Lsh(left, uint(right.Uint64())), nil
		case lk.SHIFTRIGHT:
			if !right.IsUint64() {
				return nil, msg.ValueOutOfBounds(m, n, right)
			}
			return big.NewInt(0).Rsh(left, uint(right.Uint64())), nil
		case lk.EQUALS:
			if left.Cmp(right) == 0 {
				return big.NewInt(1), nil
			} else {
				return big.NewInt(0), nil
			}
		case lk.DIFFERENT:
			if left.Cmp(right) != 0 {
				return big.NewInt(1), nil
			} else {
				return big.NewInt(0), nil
			}
		case lk.MORE:
			if left.Cmp(right) == 1 {
				return big.NewInt(1), nil
			} else {
				return big.NewInt(0), nil
			}
		case lk.MOREEQ:
			res := left.Cmp(right)
			if res == 1 || res == 0 {
				return big.NewInt(1), nil
			} else {
				return big.NewInt(0), nil
			}
		case lk.LESS:
			if left.Cmp(right) == -1 {
				return big.NewInt(1), nil
			} else {
				return big.NewInt(0), nil
			}
		case lk.LESSEQ:
			res := left.Cmp(right)
			if res == -1 || res == 0 {
				return big.NewInt(1), nil
			} else {
				return big.NewInt(0), nil
			}
		case lk.AND:
			// since left and right are either 1 or 0
			return big.NewInt(0).And(left, right), nil
		case lk.OR:
			// since left and right are either 1 or 0
			return big.NewInt(0).Or(left, right), nil
		default:
			panic("something is not right")
		}
	case lk.NOT, lk.NEG, lk.BITWISENOT:
		op, err := computeExpr(m, n.Leaves[0])
		if err != nil {
			return nil, err
		}
		switch n.Lex {
		case lk.NOT:
			if op.Cmp(zero) == 0 {
				return big.NewInt(1), nil
			} else if op.Cmp(one) == 0 {
				return big.NewInt(0), nil
			} else {
				panic("bool with weird values")
			}
		case lk.NEG:
			return big.NewInt(0).Neg(op), nil
		case lk.BITWISENOT:
			return big.NewInt(0).Not(op), nil
		}
	case lk.COLON:
		return computeColonExpr(m, n)
	}
	return nil, nil
}

// we cap it to fit inside the type given,
// but we do not overflow
func computeColonExpr(M *mod.Module, n *mod.Node) (*big.Int, *Error) {
	left := n.Leaves[1]
	tp := n.Leaves[0]

	min, max := getMinMax(tp.Type)

	op, err := computeExpr(M, left)
	if err != nil {
		return nil, err
	}

	if op.Cmp(min) == -1 {
		return big.NewInt(0).Set(min), nil
	}
	if op.Cmp(max) == 1 {
		return big.NewInt(0).Set(max), nil
	}
	return op, nil
}

func getIDValue(M *mod.Module, n *mod.Node) *big.Int {
	sy := M.GetSymbol(n.Text)
	if sy == nil {
		panic("symbol not found")
	}
	if sy.Kind != gk.Const {
		panic("not const")
	}
	return sy.Const.Value
}

func getSizeof(M *mod.Module, n *mod.Node) (*big.Int, *Error) {
	op := n.Leaves[0]
	dot := n.Leaves[1]
	var sy *mod.Global
	switch op.Lex {
	case lk.IDENTIFIER: // struct or data
		sy = M.GetSymbol(op.Text)
	case lk.DOUBLECOLON: // external struct or data
		moduleName := op.Leaves[0].Text
		symbolName := op.Leaves[1].Text
		sy = M.GetExternalSymbol(moduleName, symbolName)
	default: // basic or proc type
		return op.Type.Sizeof(), nil
	}
	if dot != nil {
		id := dot.Leaves[0].Text
		t := sy.Struct.Type
		field, ok := t.Struct.Field(id)
		if !ok {
			panic("this must be safe on this pass")
		}
		out := int64(field.Type.Size())
		return big.NewInt(out), nil
	} else {
		// can only be struct or data
		if sy.Kind == gk.Struct {
			return sy.Struct.Type.Sizeof(), nil
		} else {
			return sy.Data.Size, nil
		}
	}
}

func getDotAccess(M *mod.Module, n *mod.Node) (*big.Int, *Error) {
	op := n.Leaves[1]
	id := n.Leaves[0].Text

	var sy *mod.Global
	switch op.Lex {
	case lk.IDENTIFIER: // data declaration
		sy = M.GetSymbol(op.Text)
	case lk.DOUBLECOLON: // external data declaration
		modName := op.Leaves[0].Text
		symName := op.Leaves[1].Text
		sy = M.GetExternalSymbol(modName, symName)
	default:
		panic("non-const expression")
	}

	t := sy.Struct.Type
	field, ok := t.Struct.Field(id)
	if !ok {
		panic("this must be safe on this pass")
	}
	return field.Offset, nil
}

func getExternalSymbol(M *mod.Module, n *mod.Node) *big.Int {
	module := n.Leaves[0].Text
	id := n.Leaves[1].Text
	sy := M.GetExternalSymbol(module, id)
	switch sy.Kind {
	case gk.Data:
		return sy.Data.Size
	case gk.Const:
		return sy.Const.Value
	}
	panic("should not reach here")
}

func stringSize(oldtext string) int {
	text := oldtext[1 : len(oldtext)-1]
	size := 0
	for i := 0; i < len(text); i++ {
		if text[i] == '\\' {
			i++
		}
		size += 1
	}
	return size
}

func badConstExpr(M *mod.Module, n *mod.Node) *Error {
	msg := "constant expression had nil value"
	return util.NewInternalError(M, n, msg)
}

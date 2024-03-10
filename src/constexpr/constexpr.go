package constexpr

import (
	"math/big"
	. "mpc/core"
	mod "mpc/core/module"
	lk "mpc/core/module/lexkind"
	sk "mpc/core/module/symbolkind"
	util "mpc/core/util"
	msg "mpc/messages"
	T "mpc/pir/types"
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
			err = evalSymbol(m, sy)
			if err != nil {
				return err
			}
		}
	}
	return nil
}

func evalSymbol(m *mod.Module, sy *mod.Symbol) *Error {
	if sy.Visited {
		return nil
	}
	sy.Visited = true
	err := evalDeps(m, sy)
	if err != nil {
		return err
	}

	switch sy.T {
	case sk.Const:
		err = evalConst(m, sy)
	case sk.Data:
		err = evalData(m, sy)
	}
	if err != nil {
		return err
	}
	return nil
}

func evalDeps(m *mod.Module, sy *mod.Symbol) *Error {
	for _, ref := range sy.Refs {
		err := evalSymbol(m, ref)
		if err != nil {
			return err
		}
	}
	return nil
}

func evalConst(m *mod.Module, sy *mod.Symbol) *Error {
	v, err := compute(m, sy.N.Leaves[1])
	if err != nil {
		return err
	}
	sy.Const.Value = v
	return nil
}

func evalData(m *mod.Module, sy *mod.Symbol) *Error {
	arg := sy.N.Leaves[2]
	switch arg.Lex {
	case lk.STRING_LIT:
		size := stringSize(arg.Text)
		sy.Data.Size = big.NewInt(int64(size))
		sy.Data.Contents = arg.Text
		return nil
	case lk.BLOB:
		return evalBlob(m, sy, arg)
	default:
		v, err := compute(m, arg)
		if err != nil {
			return err
		}
		sy.Data.Size = v
		return nil
	}
}

func evalBlob(m *mod.Module, sy *mod.Symbol, n *mod.Node) *Error {
	blob := n.Leaves[1]

	nums := make([]*big.Int, len(blob.Leaves))
	for i, leaf := range blob.Leaves {
		num, err := compute(m, leaf)
		if err != nil {
			return err
		}
		nums[i] = num
	}
	sy.Data.Nums = nums
	sy.Data.Size = big.NewInt(int64(sy.Data.DataType.Size() * len(nums)))
	return nil
}

func compute(m *mod.Module, n *mod.Node) (*big.Int, *Error) {
	min, max := getMinMax(n.T)
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
		panic("unimplemented")
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

	min, max := getMinMax(tp.T)

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
	if sy.T != sk.Const {
		panic("not const")
	}
	return sy.Const.Value
}

// TODO: deal with STRUCT.FIELDS
func getSizeof(M *mod.Module, n *mod.Node) (*big.Int, *Error) {
	op := n.Leaves[1]
	switch op.Lex {
	case lk.IDENTIFIER: // data declaration
		sy := M.GetSymbol(op.Text)
		return sy.Data.Size, nil
	case lk.DOUBLECOLON: // external data declaration
		modName := op.Leaves[0].Text
		symName := op.Leaves[1].Text
		sy := M.GetExternalSymbol(modName, symName)
		return sy.Data.Size, nil
	default:
		return big.NewInt(int64(op.T.Size())), nil
	}
}

func getExternalSymbol(M *mod.Module, n *mod.Node) *big.Int {
	module := n.Leaves[0].Text
	id := n.Leaves[1].Text
	sy := M.GetExternalSymbol(module, id)
	switch sy.T {
	case sk.Data:
		return sy.Data.Size
	case sk.Const:
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

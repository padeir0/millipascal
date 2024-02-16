// scanning register allocator (Here be dragons.)
//
// be aware that procedures prefixed with "_" depend on the context
// of the calling procedure, that is, they're meant to be called
// inside other specific procedures and should not be used freely.

package resalloc

import (
	"mpc/pir"
	pc "mpc/pir/class"
	pfk "mpc/pir/flowkind"
	pik "mpc/pir/instrkind"

	mir "mpc/pir/backends/linuxamd64/mir"
	mc "mpc/pir/backends/linuxamd64/mir/class"
	mfk "mpc/pir/backends/linuxamd64/mir/flowkind"
	mik "mpc/pir/backends/linuxamd64/mir/instrkind"

	T "mpc/pir/types"

	IRU "mpc/pir/backends/linuxamd64/mir/util"

	"math/big"
	"strconv"
	"strings"
)

type reg int64
type spill int64
type calleeInterproc int64
type callerInterproc int64

type value struct {
	Class pc.Class
	ID    int64
}

func (v value) String() string {
	return v.Class.String() + " " + strconv.FormatInt(v.ID, 10)
}

type stack struct {
	items []int
	top   int
}

func (s *stack) String() string {
	output := []string{}
	for _, item := range s.items[:s.top+1] {
		output = append(output, strconv.Itoa(item))
	}
	return "[" + strings.Join(output, ", ") + "]"
}

func newStack(size int) *stack {
	items := make([]int, size)
	for i := range items {
		items[i] = size - i - 1
	}
	return &stack{
		items: items,
		top:   size - 1,
	}
}

func (s *stack) HasItems() bool {
	return s.top >= 0
}

func (s *stack) Push(i int) {
	s.top++
	if s.top >= len(s.items) {
		s.items = append(s.items, make([]int, 2)...)
	}
	s.items[s.top] = i
}

func (s *stack) Pop() int {
	if s.top < 0 {
		return s.items[0]
	}
	item := s.items[s.top]
	s.top--
	return item
}

func (s *stack) Size() int {
	return s.top
}

type StorageClass int

func (s StorageClass) String() string {
	switch s {
	case Register:
		return "register"
	case Local:
		return "local"
	case CalleeInterProc:
		return "callee_interproc"
	case CallerInterProc:
		return "caller_interproc"
	case Spill:
		return "spill"
	}
	return "?"
}

func (s StorageClass) IsAddressable() bool {
	return s == Local || s == Spill || s == CalleeInterProc || s == CallerInterProc
}

func (s StorageClass) ToMirc() mc.Class {
	switch s {
	case Register:
		return mc.Register
	case Local:
		return mc.Local
	case CalleeInterProc:
		return mc.CalleeInterproc
	case Spill:
		return mc.Spill
	}
	return mc.InvalidMIRClass
}

const (
	InvalidStorageClass StorageClass = iota

	Register
	Local
	CalleeInterProc
	CallerInterProc
	Spill
)

type useInfo struct {
	Place   StorageClass
	Num     int64
	T       *T.Type
	Mutated bool
}

func (u useInfo) String() string {
	return u.Place.String() + " " + strconv.FormatInt(u.Num, 10) + " " + u.T.String()
}

func (u useInfo) IsValid() bool {
	if u.Place == InvalidStorageClass {
		return false
	}
	if u.Num < 0 {
		return false
	}
	if u.T == nil {
		return false
	}
	return true
}

type deferredInstr struct {
	index int
	instr *mir.Instr
}

type state struct {
	AvailableRegs *stack
	// UsedRegs[ reg ] retuns the value stored in the register
	UsedRegs map[reg]value

	AvailableSpills *stack
	// UsedSpills[ spill ] retuns the value stored in the spill address
	UsedSpills map[spill]value

	// LiveValues[ value ] retuns the register, interproc or spill storing this value
	LiveValues map[value]useInfo

	// stores the index of the furthest use of each value
	valueUse map[value]int

	MaxCalleeInterproc int

	hirProgram *pir.Program
	hirBlock   *pir.BasicBlock
	hirProc    *pir.Procedure

	outputBlock *mir.BasicBlock
	outputProc  *mir.Procedure
}

func newState(program *pir.Program, numRegs int) *state {
	return &state{
		AvailableRegs: newStack(numRegs),
		UsedRegs:      map[reg]value{},

		AvailableSpills: newStack(16),
		UsedSpills:      map[spill]value{},

		LiveValues: map[value]useInfo{},
		valueUse:   map[value]int{},

		hirProgram: program,
	}
}

func (s *state) ExpectValue(v value) useInfo {
	info, ok := s.LiveValues[v]
	if !ok {
		panic("value not found: " + v.String())
	}
	return info
}

func (s *state) Mark(v value) {
	info, ok := s.LiveValues[v]
	if !ok {
		panic("marking dead value!")
	}
	if v.Class == pc.Local || v.Class == pc.Arg {
		info.Mutated = true
		s.LiveValues[v] = info
	}
}

func (s *state) HasFreeRegs() bool {
	return s.AvailableRegs.HasItems()
}

func (s *state) AmountFreeRegs() int {
	return s.AvailableRegs.Size()
}

func (s *state) Free(v value) {
	loc, ok := s.LiveValues[v]
	if !ok {
		panic("freeing unfound value")
	}
	delete(s.LiveValues, v)

	switch loc.Place {
	case Register:
		r := reg(loc.Num)
		s.FreeReg(r)
	case Spill:
		a := spill(loc.Num)
		s.FreeSpill(a)
	case CalleeInterProc:
		// no need to keep track of this
	case Local, CallerInterProc:
		panic("freeing " + loc.Place.String())
	}
}

func (s *state) FreeReg(r reg) {
	_, ok := s.UsedRegs[r]
	if ok {
		delete(s.UsedRegs, r)
		s.AvailableRegs.Push(int(r))
		return
	}
	panic("freeing unused register: " + strconv.FormatInt(int64(r), 10))
}

func (s *state) FreeSpill(a spill) {
	_, ok := s.UsedSpills[a]
	if ok {
		delete(s.UsedSpills, a)
		s.AvailableSpills.Push(int(a))
		return
	}
	panic("freeing unused spill")
}

func (s *state) AllocReg(v value, t *T.Type) reg {
	info, ok := s.LiveValues[v]
	if ok && info.Place == Register {
		// this should be fine, live values shouldn't be corrupt
		return reg(info.Num)
	}
	r := reg(s.AvailableRegs.Pop())
	s.UsedRegs[r] = v
	s.LiveValues[v] = useInfo{Place: Register, Num: int64(r), T: t}
	return r
}

func (s *state) FurthestUse(index int) (useInfo, value) {
	biggestIndex := index
	var outputInfo useInfo
	var outputValue value
	for v, info := range s.LiveValues {
		lastUse := s.valueUse[v]
		if info.Place == Register && lastUse > biggestIndex {
			biggestIndex = lastUse

			outputInfo = info
			outputValue = v
		}
	}

	return outputInfo, outputValue
}

func (s *state) Spill(r reg, t *T.Type) spill {
	v, ok := s.UsedRegs[r]
	if !ok {
		sreg := strconv.Itoa(int(r))
		panic("spilling unused register: " + sreg)
	}
	useinfo := s.LiveValues[v]
	s.FreeReg(r)
	a := spill(s.AvailableSpills.Pop())
	s.UpdateMaxSpill(int(a) + 1)
	s.UsedSpills[a] = v
	s.LiveValues[v] = useInfo{Place: Spill, Num: int64(a), T: t, Mutated: useinfo.Mutated}
	return a
}

func (s *state) UpdateMaxCalleeInterproc(numargs int, numrets int) {
	if numargs > s.MaxCalleeInterproc {
		s.MaxCalleeInterproc = numargs
	}
	if numrets > s.MaxCalleeInterproc {
		s.MaxCalleeInterproc = numrets
	}
}

func (s *state) UpdateMaxSpill(spill int) {
	if spill > s.outputProc.NumOfSpills {
		s.outputProc.NumOfSpills = spill
	}
}

// must preserve insertion order
func (s *state) AddInstr(instr mir.Instr) {
	s.outputBlock.Code = append(s.outputBlock.Code, instr)
}

func (s *state) String() string {
	livevalues := "Live Values ["
	for value, useinfo := range s.LiveValues {
		livevalues += "(" + value.String() + ", " + useinfo.String() + ") "
	}
	livevalues += "]"

	registers := "Used Regs ["
	for r, v := range s.UsedRegs {
		rStr := strconv.FormatInt(int64(r), 10)
		vStr := v.String()
		registers += "(" + rStr + ", " + vStr + ")"
	}
	registers += "]"

	return livevalues + "\n" + registers
}

func Allocate(P *pir.Program, numRegs int) *mir.Program {
	output := &mir.Program{
		Name:    P.Name,
		Entry:   mir.SymbolID(P.Entry),
		Symbols: make([]*mir.Symbol, len(P.Symbols)),
	}
	for i, sy := range P.Symbols {
		if sy.Builtin {
			output.Symbols[i] = allocBuiltin(sy)
		} else {
			if sy.Proc != nil {
				proc := allocProc(P, sy.Proc, numRegs)
				output.Symbols[i] = &mir.Symbol{Proc: proc}
			}
			if sy.Mem != nil {
				mem := hirToMirMem(sy.Mem)
				output.Symbols[i] = &mir.Symbol{Mem: mem}
			}
		}
	}
	return output
}

func allocBuiltin(sy *pir.Symbol) *mir.Symbol {
	return &mir.Symbol{
		Proc:    &mir.Procedure{Label: sy.Proc.Label},
		Builtin: true,
	}
}

func allocProc(Program *pir.Program, proc *pir.Procedure, numRegs int) *mir.Procedure {
	outProc := hirToMirProc(proc)
	outProc.AllBlocks = make([]*mir.BasicBlock, len(proc.AllBlocks))
	outProc.NumOfSpills = 0
	for i, curr := range proc.AllBlocks {
		s := newState(Program, numRegs)
		s.outputBlock = hirToMirBlock(curr)
		s.outputProc = outProc
		s.hirProc = proc
		s.hirBlock = curr
		findUses(s)
		allocBlock(s)
		calcRegions(s)
		// preserve the BlockIDs
		outProc.AllBlocks[i] = s.outputBlock
	}
	outProc.NumOfVars = len(proc.Vars)
	return outProc
}

func calcRegions(s *state) {
	if s.outputProc.NumOfMaxCalleeArguments < s.MaxCalleeInterproc {
		s.outputProc.NumOfMaxCalleeArguments = s.MaxCalleeInterproc
	}
}

func transformFlow(s *state) {
	switch s.hirBlock.Out.T {
	case pfk.Return:
		transformReturn(s)
	case pfk.Exit, pfk.If:
		s.outputBlock.Out.T = hirToMirFlow(s.hirBlock.Out.T)
		s.outputBlock.Out.V = []mir.Operand{toMirc(s, s.hirBlock.Out.V[0])}
	}
}

// TODO: OPT: transformReturn should look if the value is already where it needs to be (in the respective Caller Interproc)
func transformReturn(s *state) {
	type RetVal struct {
		Index int64
		Op    pir.Operand
	}

	notAlive := []RetVal{}
	// load the already immediate ones first
	for i, ret := range s.hirBlock.Out.V {
		rVal := toValue(ret)
		info, ok := s.LiveValues[rVal]
		if ok && info.Place == Register {
			moveArgIfNeeded(s, i)
			regOp := newRegOp(reg(info.Num), info.T)
			callerInterproc := newOp(ret.Type, mc.CallerInterproc, int64(i), nil)
			loadRet := IRU.Store(regOp, callerInterproc)
			s.AddInstr(loadRet)
			s.Free(rVal)
		} else {
			rv := RetVal{Index: int64(i), Op: ret}
			notAlive = append(notAlive, rv)
		}
	}

	end := len(s.hirBlock.Code)
	// then load the remaining
	for _, ret := range notAlive {
		moveArgIfNeeded(s, int(ret.Index))
		immediateRet := ensureImmediate(s, end, ret.Op)
		callerInterproc := newOp(ret.Op.Type, mc.CallerInterproc, ret.Index, nil)
		loadRet := IRU.Store(immediateRet, callerInterproc)
		s.AddInstr(loadRet)
	}
	s.outputBlock.Out.V = nil
}

func moveArgIfNeeded(s *state, pos int) {
	if pos >= len(s.hirProc.Args) {
		return
	}
	op := pir.Operand{
		Class: pc.Arg,
		Type:  s.hirProc.Args[pos],
		ID:    int64(pos),
	}
	v := toValue(op)
	index := retIndex + pos
	end := len(s.hirBlock.Code)
	if isNeeded(s, index, v) {
		ensureImmediate(s, end, op) // this way the regalloc keeps track of it
	}
}

// index of the return in a block
const retIndex = 1 << 30

func findUses(s *state) {
	for index, instr := range s.hirBlock.Code {
		vals := getUsedValues(instr)
		for _, v := range vals {
			s.valueUse[v] = index
		}
	}

	// check if value is returned or used in branching
	for i, op := range s.hirBlock.Out.V {
		if op.Class == pc.Temp || op.Class == pc.Local || op.Class == pc.Arg {
			v := toValue(op)
			s.valueUse[v] = retIndex + i
		}
	}
}

func getUsedValues(instr pir.Instr) []value {
	output := []value{}
	for _, op := range instr.Operands {
		if op.Class == pc.Temp || op.Class == pc.Local || op.Class == pc.Arg {
			output = append(output, toValue(op))
		}
	}
	for _, dest := range instr.Destination {
		if dest.Class == pc.Temp || dest.Class == pc.Local || dest.Class == pc.Arg {
			output = append(output, toValue(dest))
		}
	}
	return output
}

func allocBlock(s *state) *mir.BasicBlock {
	for i, instr := range s.hirBlock.Code {
		switch instr.T {
		case pik.Add, pik.Sub, pik.Mult, pik.Div, pik.Rem,
			pik.Eq, pik.Diff, pik.Less,
			pik.More, pik.LessEq, pik.MoreEq,
			pik.Or, pik.And, pik.Xor, pik.ShiftLeft, pik.ShiftRight:
			allocBinary(s, instr, i)
		case pik.Not, pik.Neg,
			pik.Convert, pik.LoadPtr:
			allocUnary(s, instr, i)
		case pik.StorePtr:
			allocStorePtr(s, instr, i)
		case pik.Copy:
			allocCopy(s, instr, i)
		case pik.Call:
			allocCall(s, instr, i)
		}
	}
	if !s.hirBlock.IsTerminal() {
		storeLiveLocals(s)
	}
	transformFlow(s)
	return s.outputBlock
}

func allocBinary(s *state, instr pir.Instr, index int) {
	a := instr.Operands[0]
	b := instr.Operands[1]
	c := instr.Destination[0]

	outInstr := hirToMirInstr(instr)
	outInstr.A = mir.OptOperand_(ensureImmediate(s, index, a))
	outInstr.B = mir.OptOperand_(ensureImmediate(s, index, b))

	freeIfNotNeededAndNotMutated(s, index, instr, toValue(a))
	freeIfNotNeededAndNotMutated(s, index, instr, toValue(b))

	cv := toValue(c)
	outInstr.Dest = mir.OptOperand_(ensureImmediate(s, index, c))
	s.Mark(cv)

	s.AddInstr(outInstr)
}

func allocUnary(s *state, instr pir.Instr, index int) {
	a := instr.Operands[0]
	c := instr.Destination[0]

	outInstr := hirToMirInstr(instr)
	outInstr.A = mir.OptOperand_(ensureImmediate(s, index, a))
	freeIfNotNeededAndNotMutated(s, index, instr, toValue(a))

	cv := toValue(c)
	outInstr.Dest = mir.OptOperand_(ensureImmediate(s, index, c))
	s.Mark(cv)

	s.AddInstr(outInstr)
}

func allocStorePtr(s *state, instr pir.Instr, index int) {
	a := instr.Operands[0]
	b := instr.Operands[1]
	outInstr := hirToMirInstr(instr)
	outInstr.A = mir.OptOperand_(ensureImmediate(s, index, a))
	outInstr.B = mir.OptOperand_(ensureImmediate(s, index, b))
	freeIfNotNeededAndNotMutated(s, index, instr, toValue(a))
	freeIfNotNeededAndNotMutated(s, index, instr, toValue(b))

	s.AddInstr(outInstr)
}

// Combination of possible Copy instructions
// Notation is: hirc (mirc) -> hirc (mirc)
// 	temp (spill|reg|calleeInter) -> temp (reg)
// 	temp (spill|reg|calleeInter) -> local (reg|local)
// 	temp (spill|reg|calleeInter) -> arg (reg|callerInter)
// 	local (reg|local) -> temp (reg)
// 	local (reg|local) -> local (reg|local)
// 	local (reg|local) -> arg (reg|callerInter)
// 	arg (reg|callerInter) -> temp (reg)
// 	arg (reg|callerInter) -> local (reg|local)
// 	arg (reg|callerInter) -> arg (reg|callerInter)
// 	global (static) -> temp (reg)
// 	global (static) -> local
// 	global (static) -> arg (reg|callerInter)
// 	lit (lit) -> temp (reg)
// 	lit (lit) -> local (reg|local)
// 	lit (lit) -> arg (reg|callerInter)
// c.HirC can only be Temp, Local or Arg
func allocCopy(s *state, instr pir.Instr, index int) {
	source := instr.Operands[0]
	sourceIsAddr := isAddressable(s, source)

	dest := instr.Destination[0]
	destIsAddr := isAddressable(s, dest)

	outInstr := hirToMirInstr(instr)

	if sourceIsAddr {
		if destIsAddr {
			// TODO: OPT: see if value is already in register
			// LOAD  source -> reg
			// STORE reg    -> dest
			outInstr.T = mik.Load
			outInstr.A = toMircOpt(s, source)
			reg := _allocReg(s, toValue(source), source.Type, index)
			outInstr.Dest = mir.OptOperand_(reg)
			s.AddInstr(outInstr)

			destMirc := toMirc(s, dest)
			corruptOldVersion(s, dest)
			s.AddInstr(IRU.Store(reg, destMirc))
		} else {
			// LOAD source -> dest
			outInstr.T = mik.Load
			outInstr.A = toMircOpt(s, source)
			outInstr.Dest = toMircOpt(s, dest)
			s.Mark(toValue(dest))

			s.AddInstr(outInstr)
		}
	} else {
		if destIsAddr {
			// STORE source -> dest
			outInstr.T = mik.Store
			outInstr.A = toMircOpt(s, source)
			outInstr.Dest = toMircOpt(s, dest)
			corruptOldVersion(s, dest)

			s.AddInstr(outInstr)
		} else {
			// COPY source -> dest
			outInstr.A = toMircOpt(s, source)
			outInstr.Dest = toMircOpt(s, dest)
			s.Mark(toValue(dest))

			s.AddInstr(outInstr)
		}
	}
	res, ok := freeIfNotNeeded(s, index, toValue(source))
	if ok {
		s.AddInstr(res)
	}
}

func corruptOldVersion(s *state, op pir.Operand) {
	v := toValue(op)
	info, ok := s.LiveValues[v]
	if ok && info.Place == Register {
		s.Free(v)
	}
}

// transforms call instructions from:
// 	call <proc>, <arg1>, ..., <argN> -> <ret1>, ..., <retN>
// to:
// 	store arg1 -> interproc1
// 	...
// 	store argN -> interprocN
// 	call <proc>
//
// ret1 is assumed to be in interproc1
// retN is assumed to be in interprocN
func allocCall(s *state, instr pir.Instr, index int) {
	// TODO: OPT: spillAllLiveInterproc should only spill the ones being corrupted
	spillAllLiveRegisters(s, index) // these need to be in order
	spillAllLiveInterproc(s, index)
	loadArguments(s, instr, index)
	// TODO: OPT: clearVolatiles should only clear the ones being corrupted
	clearVolatiles(s)

	outInstr := hirToMirInstr(instr)
	outInstr.A = mir.OptOperand_(ensureImmediate(s, index, instr.Operands[0]))
	s.AddInstr(outInstr)

	for i, dest := range instr.Destination {
		v := toValue(dest)
		callee := calleeInterproc(i)
		switch dest.Class {
		case pc.Temp:
			s.LiveValues[v] = useInfo{Place: CalleeInterProc, Num: int64(i), T: dest.Type}
		case pc.Arg:
			load, op := loadCalleeInterproc(s, callee, v, dest.Type, index)
			s.AddInstr(load)
			r := reg(op.ID)
			arg := callerInterproc(v.ID)
			store := storeArg(r, arg, dest.Type)
			s.AddInstr(store)
		case pc.Local:
			load, op := loadCalleeInterproc(s, callee, v, dest.Type, index)
			s.AddInstr(load)
			r := reg(op.ID)
			store := storeLocal(r, v.ID, dest.Type)
			s.AddInstr(store)
		}
	}
	for _, op := range instr.Operands {
		freeIfNotNeeded(s, index, toValue(op))
	}

	s.UpdateMaxCalleeInterproc(len(instr.Operands)-1, len(instr.Destination))
}

func clearVolatiles(s *state) {
	toFree := []value{}
	for val, info := range s.LiveValues {
		if info.Place != Local && info.Place != Spill {
			toFree = append(toFree, val)
		}
	}
	for _, v := range toFree {
		s.Free(v)
	}
}

func loadArguments(s *state, instr pir.Instr, index int) {
	// ensure immediate, then store
	for i, op := range instr.Operands[1:] {
		v := toValue(op)
		info, ok := s.LiveValues[v]
		if ok && info.Place == CalleeInterProc && info.Num == int64(i) {
			// if it's already where it needs to be
			continue
		}
		immediate := ensureImmediate(s, index, op)
		arg := newOp(op.Type, mc.CalleeInterproc, int64(i), nil)
		storeArg := IRU.Store(immediate, arg)
		s.AddInstr(storeArg)

		freeIfNotNeededAndNotMutated(s, index, instr, v)
	}
}

func isAddressable(s *state, o pir.Operand) bool {
	switch o.Class {
	case pc.Temp:
		info, ok := s.LiveValues[toValue(o)]
		if ok {
			return info.Place.IsAddressable()
		}
		panic("isAddressable: temp is not alive")
	case pc.Lit, pc.Global:
		return false
	case pc.Local, pc.Arg:
		return true
	}
	panic("isAddressable: wtf")
}

func toMircOpt(s *state, o pir.Operand) mir.OptOperand {
	return mir.OptOperand_(toMirc(s, o))
}

func toMirc(s *state, o pir.Operand) mir.Operand {
	switch o.Class {
	case pc.Temp:
		info, ok := s.LiveValues[toValue(o)]
		if ok {
			return newOp(o.Type, info.Place.ToMirc(), info.Num, nil)
		}
		panic("toMirc: temp is not alive")
	case pc.Local:
		return newOp(o.Type, mc.Local, o.ID, o.Num)
	case pc.Arg:
		return newOp(o.Type, mc.CallerInterproc, o.ID, o.Num)
	case pc.Global:
		return newOp(o.Type, mc.Static, o.ID, o.Num)
	case pc.Lit:
		return newOp(o.Type, mc.Lit, o.ID, o.Num)
	}
	panic("toMirc: wtf")
}

func storeLiveLocals(s *state) {
	for val, info := range s.LiveValues {
		if info.Place == Register && info.Mutated {
			if val.Class == pc.Local {
				r := reg(info.Num)
				instr := storeLocal(r, val.ID, info.T)
				s.AddInstr(instr)
			}
			if val.Class == pc.Arg {
				r := reg(info.Num)
				it := callerInterproc(val.ID)
				instr := storeArg(r, it, info.T)
				s.AddInstr(instr)
			}
		}
	}
}

func spillAllLiveInterproc(s *state, index int) {
	for val, info := range s.LiveValues {
		lastUse := s.valueUse[val]
		if info.Place == CalleeInterProc && lastUse >= index {
			callee := calleeInterproc(info.Num)

			// if we're using in the current instruction,
			// we try to just giggle it to a register
			instr, op := loadCalleeInterproc(s, callee, val, info.T, index)
			s.AddInstr(instr)

			// however, if we have few available registers,
			// or if we're not using in this instruction,
			// we spill it, consequentially freeing the register
			// we keep at least 2 registers for load/store operations
			if s.AmountFreeRegs() < 3 || lastUse > index {
				r := reg(op.ID)
				spill := spillTemp(s, r, info.T)
				s.AddInstr(spill)
			}
		}
	}
}

func spillAllLiveRegisters(s *state, index int) {
	for val, info := range s.LiveValues {
		if info.Place == Register && (isNeeded(s, index, val) || info.Mutated) {
			switch val.Class {
			case pc.Local:
				r := reg(info.Num)
				s.AddInstr(storeLocal(r, val.ID, info.T))
			case pc.Arg:
				r := reg(info.Num)
				arg := callerInterproc(val.ID)
				s.AddInstr(storeArg(r, arg, info.T))
			case pc.Temp:
				s.AddInstr(spillTemp(s, reg(info.Num), info.T))
			}
		}
	}
}

func ensureImmediate(s *state, index int, op pir.Operand) mir.Operand {
	if op.Class == pc.Lit || op.Class == pc.Global {
		return toMirc(s, op)
	}
	v := toValue(op)
	t := op.Type
	return _ensureImmediate(s, index, v, t)
}

func _ensureImmediate(s *state, index int, v value, t *T.Type) mir.Operand {
	info, ok := s.LiveValues[v]
	if ok {
		switch info.Place {
		case Register:
			return newRegOp(reg(info.Num), t)
		case Spill:
			instr, op := _loadSpill(s, v, info, index)
			s.AddInstr(instr)
			return op
		case CalleeInterProc:
			callee := calleeInterproc(info.Num)
			instr, op := _loadCalleeInterproc(s, callee, v, info.T, index)
			s.AddInstr(instr)
			return op
		case CallerInterProc:
			caller := callerInterproc(info.Num)
			instr, op := _loadCallerInterproc(s, v, caller, info.T, index)
			s.AddInstr(instr)
			return op
		case Local:
			instr, op := _loadLocal(s, v, info.T, index)
			s.AddInstr(instr)
			return op
		}
		panic("ensureImmediate: Invalid StorageClass")
	}
	switch v.Class {
	case pc.Temp:
		return _allocReg(s, v, t, index)
	case pc.Local:
		instr, op := _loadLocal(s, v, t, index)
		s.AddInstr(instr)
		return op
	case pc.Arg:
		instr, op := _loadArg(s, v, t, index)
		s.AddInstr(instr)
		return op
	}
	panic("ensureImmediate: Invalid HIRClass")
}

func newOp(t *T.Type, m mc.Class, id int64, num *big.Int) mir.Operand {
	if m == mc.Lit && num == nil {
		panic("malformed literal")
	}
	return mir.Operand{
		Class: m,
		Type:  t,
		ID:    id,
		Num:   num,
	}
}

func loadCalleeInterproc(s *state, callee calleeInterproc, v value, t *T.Type, index int) (mir.Instr, mir.Operand) {
	newOp := newCalleeInterprocOperand(callee, t)
	rOp := _ensureImmediate(s, index, v, t)
	load := IRU.Load(newOp, rOp)
	return load, rOp
}

// should only be used inside _ensureImmediate
func _loadCalleeInterproc(s *state, callee calleeInterproc, v value, t *T.Type, index int) (mir.Instr, mir.Operand) {
	newOp := newCalleeInterprocOperand(callee, t)
	rOp := _allocReg(s, v, t, index)
	load := IRU.Load(newOp, rOp)
	return load, rOp
}

// should only be used inside _ensureImmediate
func _loadCallerInterproc(s *state, v value, caller callerInterproc, t *T.Type, index int) (mir.Instr, mir.Operand) {
	newOp := newCallerInterprocOperand(caller, t)
	rOp := _allocReg(s, v, t, index)
	load := IRU.Load(newOp, rOp)
	return load, rOp
}

func _loadLocal(s *state, v value, t *T.Type, index int) (mir.Instr, mir.Operand) {
	newOp := newLocalOperand(v.ID, t)
	rOp := _allocReg(s, v, t, index)
	load := IRU.Load(newOp, rOp)
	return load, rOp
}

func _loadArg(s *state, v value, t *T.Type, index int) (mir.Instr, mir.Operand) {
	newOp := newCallerInterprocOperand(callerInterproc(v.ID), t)
	rOp := _allocReg(s, v, t, index)
	load := IRU.Load(newOp, rOp)
	return load, rOp
}

func _loadSpill(s *state, v value, info useInfo, index int) (mir.Instr, mir.Operand) {
	sp := spill(info.Num)
	newOp := newSpillOperand(sp, info.T)
	s.FreeSpill(sp)
	rOp := _allocReg(s, v, info.T, index)
	load := IRU.Load(newOp, rOp)
	return load, rOp
}

func _allocReg(s *state, v value, t *T.Type, index int) mir.Operand {
	if s.HasFreeRegs() {
		r := s.AllocReg(v, t)
		return newRegOp(r, t)
	}
	info, val := s.FurthestUse(index)
	if !info.IsValid() {
		panic("not enough registers")
	}
	switch val.Class {
	case pc.Temp:
		s.AddInstr(spillTemp(s, reg(info.Num), info.T))
	case pc.Local:
		if info.Mutated {
			s.AddInstr(storeLocal(reg(info.Num), val.ID, info.T))
		}
		s.Free(val)
	case pc.Arg:
		if info.Mutated {
			arg := callerInterproc(val.ID)
			s.AddInstr(storeArg(reg(info.Num), arg, info.T))
		}
		s.Free(val)
	case pc.Lit, pc.Global:
		panic("what the fuck are we even doing")
	}

	r2 := s.AllocReg(v, t)
	if reg(info.Num) != r2 {
		panic("spillRegister: " + s.AvailableRegs.String() + "\n")
	}
	return newRegOp(reg(info.Num), t)
}

func spillTemp(s *state, r reg, t *T.Type) mir.Instr {
	sNum := s.Spill(r, t)
	spillOp := newSpillOperand(sNum, t)
	regOp := newRegOp(r, t)
	return IRU.Store(regOp, spillOp)
}

func storeLocal(r reg, position int64, t *T.Type) mir.Instr {
	reg := newRegOp(r, t)
	loc := newLocalOperand(position, t)
	return IRU.Store(reg, loc)
}

func storeArg(r reg, num callerInterproc, t *T.Type) mir.Instr {
	reg := newRegOp(r, t)
	loc := newCallerInterprocOperand(num, t)
	return IRU.Store(reg, loc)
}

func newRegOp(r reg, t *T.Type) mir.Operand {
	return mir.Operand{
		Class: mc.Register,
		ID:    int64(r),
		Type:  t,
	}
}

func newSpillOperand(sNum spill, t *T.Type) mir.Operand {
	return mir.Operand{
		Class: mc.Spill,
		ID:    int64(sNum),
		Type:  t,
	}
}

func newLocalOperand(position int64, t *T.Type) mir.Operand {
	return mir.Operand{
		Class: mc.Local,
		Type:  t,
		ID:    position,
	}
}

func newCalleeInterprocOperand(i calleeInterproc, t *T.Type) mir.Operand {
	return mir.Operand{
		Class: mc.CalleeInterproc,
		Type:  t,
		ID:    int64(i),
	}
}

func newCallerInterprocOperand(i callerInterproc, t *T.Type) mir.Operand {
	return mir.Operand{
		Class: mc.CallerInterproc,
		Type:  t,
		ID:    int64(i),
	}
}

// can only insert free after current instruction
func freeIfNotNeededAndNotMutated(s *state, index int, instr pir.Instr, v value) {
	useInfo, ok := s.LiveValues[v]
	if !ok {
		return // already freed (i hope)
	}
	for _, dest := range instr.Destination {
		if dest.Class == v.Class && dest.ID == v.ID {
			return
		}
	}
	if isNeeded(s, index, v) {
		return
	}
	if useInfo.Mutated {
		return
	}
	s.Free(v)
}

// can only insert free after current instruction
func freeIfNotNeeded(s *state, index int, v value) (mir.Instr, bool) {
	useInfo, ok := s.LiveValues[v]
	if !ok {
		return mir.Instr{}, false // already freed (i hope)
	}
	if isNeeded(s, index, v) {
		return mir.Instr{}, false
	}
	s.Free(v)
	if !s.hirBlock.IsTerminal() { // no need to restore if is terminal
		if v.Class == pc.Local && useInfo.Mutated {
			r := reg(useInfo.Num)
			instr := storeLocal(r, v.ID, useInfo.T)
			return instr, true
		}
		if v.Class == pc.Arg && useInfo.Mutated {
			r := reg(useInfo.Num)
			arg := callerInterproc(v.ID)
			instr := storeArg(r, arg, useInfo.T)
			return instr, true
		}
	}
	return mir.Instr{}, false
}

func isNeeded(s *state, index int, v value) bool {
	lastUse := s.valueUse[v]
	if lastUse > index {
		return true
	}
	return false
}

func toValue(op pir.Operand) value {
	return value{
		Class: op.Class,
		ID:    op.ID,
	}
}

func hirToMirProc(proc *pir.Procedure) *mir.Procedure {
	return &mir.Procedure{
		Label:                   proc.Label,
		Vars:                    proc.Vars,
		Args:                    proc.Args,
		Rets:                    proc.Rets,
		Start:                   mir.BlockID(proc.Start),
		NumOfVars:               0,
		NumOfSpills:             0,
		NumOfMaxCalleeArguments: 0,
	}
}

func hirToMirInstrKind(hk pik.InstrKind) mik.InstrKind {
	switch hk {
	case pik.Add:
		return mik.Add
	case pik.Sub:
		return mik.Sub
	case pik.Neg:
		return mik.Neg
	case pik.Div:
		return mik.Div
	case pik.Mult:
		return mik.Mult
	case pik.Rem:
		return mik.Rem
	case pik.Eq:
		return mik.Eq
	case pik.Diff:
		return mik.Diff
	case pik.Less:
		return mik.Less
	case pik.More:
		return mik.More
	case pik.LessEq:
		return mik.LessEq
	case pik.MoreEq:
		return mik.MoreEq
	case pik.Or:
		return mik.Or
	case pik.Xor:
		return mik.Xor
	case pik.And:
		return mik.And
	case pik.Not:
		return mik.Not
	case pik.ShiftLeft:
		return mik.ShiftLeft
	case pik.ShiftRight:
		return mik.ShiftRight
	case pik.Convert:
		return mik.Convert
	case pik.Copy:
		return mik.Copy
	case pik.LoadPtr:
		return mik.LoadPtr
	case pik.StorePtr:
		return mik.StorePtr
	case pik.Call:
		return mik.Call
	}
	panic("unmapped hir instruction")
}

func hirToMirInstr(instr pir.Instr) mir.Instr {
	return mir.Instr{
		T:    hirToMirInstrKind(instr.T),
		Type: instr.Type,
	}
}

func hirToMirFlow(f pfk.FlowKind) mfk.FlowKind {
	switch f {
	case pfk.Jmp:
		return mfk.Jmp
	case pfk.If:
		return mfk.If
	case pfk.Return:
		return mfk.Return
	case pfk.Exit:
		return mfk.Exit
	}
	panic("invalid hirflow")
}

func hirToMirBlock(b *pir.BasicBlock) *mir.BasicBlock {
	return &mir.BasicBlock{
		Label: b.Label,
		Code:  make([]mir.Instr, len(b.Code))[:0],
		Out: mir.Flow{
			T:     hirToMirFlow(b.Out.T),
			V:     []mir.Operand{},
			True:  mir.BlockID(b.Out.True),  // we can do this because we preserve ID numbers
			False: mir.BlockID(b.Out.False), // between hir and mir
		},
		Visited: false,
	}
}

func hirToMirMem(mem *pir.DataDecl) *mir.DataDecl {
	return &mir.DataDecl{
		Label: mem.Label,
		Data:  mem.Data,
		Size:  mem.Size,
	}
}

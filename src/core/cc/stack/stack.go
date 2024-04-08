/*
|   Stack frame     |    Address
|-------------------|----------------------------
| CallerInterproc#0 | <- ARP + 16 + (#args-0)*8
| CallerInterproc#1 | <- ARP + 16 + (#args-1)*8
| ...               |
| CallerInterproc#N | <- ARP + 16
| Return Address    |
| Caller ARP        | <- ARP (rbp)
| Local#0           |
| ...               |
| Local#N           | <- ARP - (8 + N*8)
| Spill#0           |
| ...               |
| Spill#N           | <- ARP - (8 + #local*8 + N*8)
| CalleeInterproc#0 | <- ARP - (8 + #local*8 + #spill*8)
| CalleeInterproc#1 | <- ARP - (8 + #local*8 + #spill*8 + 1*8)
| ...               |
| CalleeInterproc#N | <- ARP - (8 + #local*8 + #spill*8 + N*8)
*/
package stack

const slot = 8

func CallArg(numVars, numSpills, numMaxCalleeArgs, i int) int {
	//        v jumps a slot because rbp points to the last rbp
	return -(slot + numVars*slot + numSpills*slot + (numMaxCalleeArgs-i-1)*slot)
}

func CallRet(numVars, numSpills, numMaxCalleeArgs, i int) int {
	return CallArg(numVars, numSpills, numMaxCalleeArgs, i)
}

func Spill(numVars, i int) int {
	//        v jumps a slot because rbp points to the last rbp
	return -(slot + numVars*slot + i*slot)
}

func Arg(i int) int {
	//      v must jump the slots: rbp, return address
	return 2*slot + i*slot
}

func Ret(i int) int {
	return Arg(i)
}

func Var(i int) int {
	//        v jumps a slot because rbp points to the last rbp
	return -(slot + i*slot)
}

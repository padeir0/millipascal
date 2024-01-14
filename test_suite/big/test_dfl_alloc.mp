from ioutil import put_char, put_int, put_4int, fatal
from constants import NULL, FULL, EMPTY, WORD
from dfl_alloc import Heap, ByteMap, Tail, Head, FrontLimit, BackLimit,
                      AllocFront, AllocBack, Free, Init,
					  PrintList, PrintMap, GetTotalFree

memory OOM "Out of memory\n"

memory test_ptrs 1024
const TOTAL_TEST_PTRS 128

proc main
var i
begin
	set i = 0
	Init[]
	while i < 16 begin
		test[i, AllocF, AllocF]
		Validate[]
		set i += 1
	end while
	set i = 0
	while i < 16 begin
		test[i, AllocB, AllocB]
		Validate[]
		set i += 1
	end while
	set i = 0
	while i < 16 begin
		test[i, AllocB, AllocF]
		Validate[]
		set i += 1
	end while
	set i = 0
	while i < 16 begin
		test[i, AllocF, AllocB]
		Validate[]
		set i += 1
	end while
end proc

# valida se estamos de volta no estado inicial
proc Validate
begin
	if GetTotalFree[] != Heap.size begin
		exit 2ss
	end if
	if Head@ptr != Tail@ptr begin
		exit 3ss
	end if
	if FrontLimit@ptr != Heap begin
		exit 4ss
	end if
	if BackLimit@ptr != Heap+Heap.size:ptr begin
		exit 5ss
	end if
end proc

proc AllocF[size] ptr
begin
	return AllocFront[size, 0ul, 0ul, 0ul, 0p]
end proc

proc AllocB[size] ptr
begin
	return AllocBack[size, 0us, 0ul]
end proc

const debug 0

proc test[n, alloc1:proc[i64]ptr, alloc2:proc[i64]ptr]
var i, a:ptr
begin
	set i = 0
	if debug != 0 begin
		PrintMap[]
		PrintList[]
	end if
	while i < TOTAL_TEST_PTRS begin
		set a = NULL
		if i % 5 == 0 begin
			set a = alloc1[17+n]
		end elseif i % 5 == 1 begin
			set a = alloc1[16+n]
		end elseif i % 5 == 2 begin
			set a = alloc1[11+n]
		end elseif i % 5 == 3 begin
			set a = alloc2[27-n]
		end elseif i % 5 == 4 begin
			set a = alloc2[33-n]
		end if

		if i % 4 == 0 begin
			freeN[1+(n%4)]
		end elseif i % 4 == 3 begin
			freeN[1]
		end if

		if a != NULL begin
			set (test_ptrs + (i*8):ptr)@ptr = a
		end else begin
			fatal[OOM, OOM.size]
		end if
		if debug != 0 begin
			put_char['\n']
			PrintMap[]
			PrintList[]
		end if
		set i += 1
	end while
	freeN[TOTAL_TEST_PTRS]
	if debug != 0 begin
		PrintMap[]
		PrintList[]
	end if
end proc

proc freeN[n]
var i, a:ptr
begin
	set i = 0
	while i < TOTAL_TEST_PTRS begin
		if n <= 0 begin
			return
		end if
		set a = (test_ptrs + (i*8):ptr)@ptr
		if a != NULL begin
			Free[a]
			set (test_ptrs + (i*8):ptr)@ptr = NULL
			set n -= 1
		end if
		set i += 1
	end while
end proc

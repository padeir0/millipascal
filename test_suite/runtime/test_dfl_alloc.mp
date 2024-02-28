from
	ioutil
import
	put_char, put_int, fatal, oom

from
	constants
import
	NULL, FULL, EMPTY, WORD

from
	dfl_alloc
import
	Heap, ByteMap, Tail, Head, FrontLimit, BackLimit,
    alloc_f, alloc_b, free, init,
	print_list, print_map, get_total_free

const TOTAL_TEST_PTRS = 128
data TestPtrs [TOTAL_TEST_PTRS * sizeof ptr]

proc main
var i:i64
begin
	init[]

	set i = 0
	while i < 100 begin
		free[alloc_f[50]]
		set i += 1
	end
	
	set i = 0
	while i < 16 begin
		test[i, _alloc_f, _alloc_f]
		validate[]
		set i += 1
	end
	set i = 0
	while i < 16 begin
		test[i, _alloc_b, _alloc_b]
		validate[]
		set i += 1
	end
	set i = 0
	while i < 16 begin
		test[i, _alloc_b, _alloc_f]
		validate[]
		set i += 1
	end
	set i = 0
	while i < 16 begin
		test[i, _alloc_f, _alloc_b]
		validate[]
		set i += 1
	end
end

# valida se estamos de volta no estado inicial
proc validate
begin
	if get_total_free[] != Heap.size begin
		exit 2ss
	end
	if Head@ptr != Tail@ptr begin
		exit 3ss
	end
	if FrontLimit@ptr != Heap begin
		exit 4ss
	end
	if BackLimit@ptr != Heap+Heap.size:ptr begin
		exit 5ss
	end
end

proc _alloc_f[size:i64] ptr
begin
	return alloc_f[size]
end

proc _alloc_b[size:i64] ptr
begin
	return alloc_b[size, 0uss, 0us, 0ul]
end

const debug = 0

proc test[n:i64, alloc1:proc[i64]ptr, alloc2:proc[i64]ptr]
var i:i64, a:ptr
begin
	set i = 0
	if debug != 0 begin
		print_map[]
		print_list[]
	end
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
		end

		if i % 4 == 0 begin
			free_n[1+(n%4)]
		end elseif i % 4 == 3 begin
			free_n[1]
		end

		if a != NULL begin
			set (TestPtrs + (i*8):ptr)@ptr = a
		end else begin
			oom[]
		end
		if debug != 0 begin
			put_char['\n']
			print_map[]
			print_list[]
		end
		set i += 1
	end
	free_n[TOTAL_TEST_PTRS]
	if debug != 0 begin
		print_map[]
		print_list[]
	end
end

proc free_n[n:i64]
var i:i64, a:ptr
begin
	set i = 0
	while i < TOTAL_TEST_PTRS begin
		if n <= 0 begin
			return
		end
		set a = (TestPtrs + (i*8):ptr)@ptr
		if a != NULL begin
			free[a]
			set (TestPtrs + (i*8):ptr)@ptr = NULL
			set n -= 1
		end
		set i += 1
	end
end

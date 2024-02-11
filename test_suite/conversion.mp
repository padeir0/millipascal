proc main
var c:i32
begin
	signed_positive[]
	unsigned[]
	unsigned_to_signed[]
	signed_to_unsigned[]
	signed_negative[]

	signed_to_ptr[]
end proc

proc signed_positive[]
var a:i8, b:i16, c:i32, d:i64
begin
	set a = 3ss
	set b = 7s
	set c = 15l
	set d = 31

	if a:i64 != 3 begin
		exit 1ss
	end if
	if b:i64 != 7 begin
		exit 2ss
	end if
	if c:i64 != 15 begin
		exit 3ss
	end if
	if d:i64 != 31 begin
		exit 4ss
	end if

	if a:i32 != 3l begin
		exit 5ss
	end if
	if b:i32 != 7l begin
		exit 6ss
	end if
	if c:i32 != 15l begin
		exit 7ss
	end if
	if d:i32 != 31l begin
		exit 8ss
	end if

	if a:i16 != 3s begin
		exit 9ss
	end if
	if b:i16 != 7s begin
		exit 10ss
	end if
	if c:i16 != 15s begin
		exit 11ss
	end if
	if d:i16 != 31s begin
		exit 12ss
	end if

	if a:i8 != 3ss begin
		exit 13ss
	end if
	if b:i8 != 7ss begin
		exit 14ss
	end if
	if c:i8 != 15ss begin
		exit 15ss
	end if
	if d:i8 != 31ss begin
		exit 16ss
	end if
end proc

proc unsigned[]
var a:u8, b:u16, c:u32, d:u64
begin
	set a = 3uss
	set b = 7us
	set c = 15ul
	set d = 31u

	if a:u64 != 3u begin
		exit 17ss
	end if
	if b:u64 != 7u begin
		exit 18ss
	end if
	if c:u64 != 15u begin
		exit 19ss
	end if
	if d:u64 != 31u begin
		exit 20ss
	end if

	if a:u32 != 3ul begin
		exit 21ss
	end if
	if b:u32 != 7ul begin
		exit 22ss
	end if
	if c:u32 != 15ul begin
		exit 23ss
	end if
	if d:u32 != 31ul begin
		exit 24ss
	end if

	if a:u16 != 3us begin
		exit 25ss
	end if
	if b:u16 != 7us begin
		exit 26ss
	end if
	if c:u16 != 15us begin
		exit 27ss
	end if
	if d:u16 != 31us begin
		exit 28ss
	end if

	if a:u8 != 3uss begin
		exit 29ss
	end if
	if b:u8 != 7uss begin
		exit 30ss
	end if
	if c:u8 != 15uss begin
		exit 31ss
	end if
	if d:u8 != 31uss begin
		exit 32ss
	end if
end proc

proc unsigned_to_signed[]
var a:u8, b:u16, c:u32, d:u64
begin
	set a = 3uss
	set b = 7us
	set c = 15ul
	set d = 31u

	if a:i64 != 3 begin
		exit 33ss
	end if
	if b:i64 != 7 begin
		exit 34ss
	end if
	if c:i64 != 15 begin
		exit 35ss
	end if
	if d:i64 != 31 begin
		exit 36ss
	end if

	if a:i32 != 3l begin
		exit 37ss
	end if
	if b:i32 != 7l begin
		exit 38ss
	end if
	if c:i32 != 15l begin
		exit 39ss
	end if
	if d:i32 != 31l begin
		exit 40ss
	end if

	if a:i16 != 3s begin
		exit 41ss
	end if
	if b:i16 != 7s begin
		exit 42ss
	end if
	if c:i16 != 15s begin
		exit 43ss
	end if
	if d:i16 != 31s begin
		exit 44ss
	end if

	if a:i8 != 3ss begin
		exit 45ss
	end if
	if b:i8 != 7ss begin
		exit 46ss
	end if
	if c:i8 != 15ss begin
		exit 47ss
	end if
	if d:i8 != 31ss begin
		exit 48ss
	end if
end proc

proc signed_to_unsigned[]
var a:i8, b:i16, c:i32, d:i64
begin
	set a = 3ss
	set b = 7s
	set c = 15l
	set d = 31

	if a:u64 != 3u begin
		exit 49ss
	end if
	if b:u64 != 7u begin
		exit 50ss
	end if
	if c:u64 != 15u begin
		exit 51ss
	end if
	if d:u64 != 31u begin
		exit 52ss
	end if

	if a:u32 != 3ul begin
		exit 53ss
	end if
	if b:u32 != 7ul begin
		exit 54ss
	end if
	if c:u32 != 15ul begin
		exit 55ss
	end if
	if d:u32 != 31ul begin
		exit 56ss
	end if

	if a:u16 != 3us begin
		exit 57ss
	end if
	if b:u16 != 7us begin
		exit 58ss
	end if
	if c:u16 != 15us begin
		exit 59ss
	end if
	if d:u16 != 31us begin
		exit 60ss
	end if

	if a:u8 != 3uss begin
		exit 61ss
	end if
	if b:u8 != 7uss begin
		exit 62ss
	end if
	if c:u8 != 15uss begin
		exit 63ss
	end if
	if d:u8 != 31uss begin
		exit 64ss
	end if
end proc

proc signed_negative[]
var a:i8, b:i16, c:i32, d:i64
begin
	set a = ~3ss
	set b = ~7s
	set c = ~15l
	set d = ~31

	if a:i64 != ~3 begin
		exit 65ss
	end if
	if b:i64 != ~7 begin
		exit 66ss
	end if
	if c:i64 != ~15 begin
		exit 67ss
	end if
	if d:i64 != ~31 begin
		exit 68ss
	end if

	if a:i32 != ~3l begin
		exit 69ss
	end if
	if b:i32 != ~7l begin
		exit 70ss
	end if
	if c:i32 != ~15l begin
		exit 71ss
	end if
	if d:i32 != ~31l begin
		exit 72ss
	end if

	if a:i16 != ~3s begin
		exit 73ss
	end if
	if b:i16 != ~7s begin
		exit 74ss
	end if
	if c:i16 != ~15s begin
		exit 75ss
	end if
	if d:i16 != ~31s begin
		exit 76ss
	end if

	if a:i8 != ~3ss begin
		exit 77ss
	end if
	if b:i8 != ~7ss begin
		exit 78ss
	end if
	if c:i8 != ~15ss begin
		exit 79ss
	end if
	if d:i8 != ~31ss begin
		exit 80ss
	end if
end proc

proc signed_to_ptr[]
var a:i8, b:i16, c:i32, d:i64
begin
	set a = 3ss
	set b = 7s
	set c = 15l
	set d = 31

	if a:ptr != 3p begin
		exit 81ss
	end if
	if b:ptr != 7p begin
		exit 82ss
	end if
	if c:ptr != 15p begin
		exit 83ss
	end if
	if d:ptr != 31p begin
		exit 84ss
	end if
end proc

proc unsigned_to_ptr[]
var a:u8, b:u16, c:u32, d:u64
begin
	set a = 3uss
	set b = 7us
	set c = 15ul
	set d = 3u1

	if a:ptr != 3p begin
		exit 85ss
	end if
	if b:ptr != 7p begin
		exit 86ss
	end if
	if c:ptr != 15p begin
		exit 87ss
	end if
	if d:ptr != 31p begin
		exit 88ss
	end if
end proc

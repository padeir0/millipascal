proc main
begin
    test_1[]
    test_2[]
    test_3[]
end proc

proc test_1[]
var a:i64, b:i32, c:i16, d:i8,
    e:u64, f:u32, g:u16, h:u8
begin
    set a = ((17ll%9ll)*(4ll+1ll))/(11ll-1ll)
    if a != 4ll begin
        exit 1ss
    end if
    set b = ((17l%9l)*(4l+1l))/(11l-1l)
    if b != 4l begin
        exit 2ss
    end if
    set c = ((17s%9s)*(4s+1s))/(11s-1s)
    if c != 4s begin
        exit 3ss
    end if
    set d = ((17ss%9ss)*(4ss+1ss))/(11ss-1ss)
    if d != 4ss begin
        exit 4ss
    end if

    set e = ((17ull%9ull)*(4ull+1ull))/(11ull-1ull)
    if e != 4ull begin
        exit 5ss
    end if
    set f = ((17ul%9ul)*(4ul+1ul))/(11ul-1ul)
    if f != 4ul begin
        exit 6ss
    end if
    set g = ((17us%9us)*(4us+1us))/(11us-1us)
    if g != 4us begin
        exit 7ss
    end if
    set h = ((17uss%9uss)*(4uss+1uss))/(11uss-1uss)
    if h != 4uss begin
        exit 8ss
    end if
end proc

proc test_2[]
var a, b, c, d, e, f,
    res
begin
    set a = 17
    set b = 9
    set c = 4
    set d = 1
    set e = 11
    set f = 1

    set res = ((a%b)*(c+d))/(e-f)
    if res != 4 begin
        exit 9ss
    end if
end proc

proc test_3[]
var a:u8, b:u8, c:u8, d:u8, e:u8, f:u8,
    res:u8
begin
    set a = 17uss
    set b = 9uss
    set c = 4uss
    set d = 1uss
    set e = 11uss
    set f = 1uss

    set res = ((a%b)*(c+d))/(e-f)
    if res != 4uss begin
        exit 10ss
    end if
end proc

export i8max, i8min, i16max, i16min, i32max, i32min, i64max, i64min

const i8max = (1<<64):i8
const i8min = (~(1<<64)):i8

const i16max = (1<<64):i16
const i16min = (~(1<<64)):i16

const i32max = (1<<64):i32
const i32min = (~(1<<64)):i32

const i64max = (1<<64):i64
const i64min = (~(1<<64)):i64

proc main
begin
    if i8max != ((1<<7)-1):i8 begin
        exit 1ss
    end if
    if i8min != (~(1<<7)):i8 begin
        exit 2ss
    end if
    if i16max != ((1<<15)-1):i16 begin
        exit 3ss
    end if
    if i16min != (~(1<<15)):i16 begin
        exit 4ss
    end if

    if i32max != ((1<<31)-1):i32 begin
        exit 5ss
    end if
    if i32min != (~(1<<31)):i32 begin
        exit 6ss
    end if
    if i64max != ((1<<63)-1):i64 begin
        exit 7ss
    end if
    if i64min != (~(1<<63)):i64 begin
        exit 8ss
    end if
end proc

export
    i8max, i8min,
    i16max, i16min,
    i32max, i32min,
    i64max, i64min,

const i8max = (1<<64):i8
const i8min = (~(1<<64)):i8

const i16max = (1<<64):i16
const i16min = (~(1<<64)):i16

const i32max = (1<<64):i32
const i32min = (~(1<<64)):i32

const i64max = (1l<<64l):i64
const i64min = (~(1l<<64l)):i64

proc main
begin
    if i8max != ((1ss<<7ss)-1ss):i8 begin
        exit 1ss;
    end
    if i8min != (~(1ss<<7ss)):i8 begin
        exit 2ss;
    end
    if i16max != ((1s<<15s)-1s):i16 begin
        exit 3ss;
    end
    if i16min != (~(1s<<15s)):i16 begin
        exit 4ss;
    end

    if i32max != ((1<<31)-1):i32 begin
        exit 5ss;
    end
    if i32min != (~(1<<31)):i32 begin
        exit 6ss;
    end
    if i64max != ((1l<<63l)-1l):i64 begin
        exit 7ss;
    end
    if i64min != (~(1l<<63l)):i64 begin
        exit 8ss;
    end
end

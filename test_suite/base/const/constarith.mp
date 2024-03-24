const a = sizeof[i32]
const b = a * 32
const c = a << 1

const d = 1
const e = (d + d) * (d + d) / (d + d)

const f = 0xFFFFFFFF
const g = (f * f):i32
const h = (f * ~f):i32

const i64max = (1<<63)-1
const i64min = ~(1<<63)

proc main
begin
    if a != sizeof[i32] or
        b != 32 * sizeof[i32] or
        c != sizeof[i32] << 1
    begin
        exit 1ss;
    end

    if e != 2 begin
        exit 2ss;
    end

    if g != i64max begin
        exit 3ss;
    end
    if h != i64min begin
        exit 4ss;
    end
end

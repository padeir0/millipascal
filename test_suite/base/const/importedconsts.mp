import globalconsts

const a = (globalconsts::i8min + globalconsts::i8max):i64
const b = (globalconsts::i16min + globalconsts::i16max):i64
const c = (globalconsts::i32min + globalconsts::i32max):i64
const d = globalconsts::i64min + globalconsts::i64max

proc main
begin
    if a != ~1 begin
        exit 1ss;
    end
    if b != ~1 begin
        exit 2ss;
    end
    if c != ~1 begin
        exit 3ss;
    end
    if d != ~1 begin
        exit 4ss;
    end
end

from arrays
import AI64, AI32, AI16, AI8

proc main
begin
    if sizeof[AI64] != sizeof[i64] begin
        exit 1ss;
    end
    if sizeof[AI32] != sizeof[i32] begin
        exit 2ss;
    end
    if sizeof[AI16] != sizeof[i16] begin
        exit 2ss;
    end
    if sizeof[AI8] != sizeof[i8] begin
        exit 3ss;
    end
end

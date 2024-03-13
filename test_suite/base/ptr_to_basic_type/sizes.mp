proc main
begin
    if sizeof^[ptr^i8] != sizeof[i8] begin
        exit 1ss;
    end
    if sizeof^[ptr^i16] != sizeof[i16] begin
        exit 2ss;
    end
    if sizeof^[ptr^i32] != sizeof[i32] begin
        exit 3ss;
    end
    if sizeof^[ptr^i64] != sizeof[i64] begin
        exit 4ss;
    end
    if sizeof^[ptr^ptr] != sizeof[ptr] begin
        exit 5ss;
    end

    if sizeof[ptr^i8] != sizeof[ptr] begin
        exit 6ss;
    end
    if sizeof[ptr^i16] != sizeof[ptr] begin
        exit 7ss;
    end
    if sizeof[ptr^i32] != sizeof[ptr] begin
        exit 8ss;
    end
    if sizeof[ptr^i64] != sizeof[ptr] begin
        exit 9ss;
    end
end

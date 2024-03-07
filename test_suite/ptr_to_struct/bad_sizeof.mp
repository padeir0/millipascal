type P is ptr^begin
    X:i64 {~sizeof[i64]};
    Y:i64 {0};
end

proc main
begin
    if sizeof^[P] == sizeof[ptr] begin
        exit 1ss;
    end
    exit 2ss;
end

proc main
var a:i32
begin
    if Try[10] != 10 begin
        exit 1ss;
    end
    if Try[5] != 5 begin
        exit 2ss;
    end
    if Try[100] != 100 begin
        exit 3ss;
    end
    if Try[33] != 33 begin
        exit 4ss;
    end
end

proc Try[n:i32] i32
var idd, i, q, b64:i32
begin
    set idd = 0;
    set i = n-1;
    while 0 <= i begin
        set idd += 1;

        A[q];
        B[b64, q, b64];

        set i -= 1;
    end
    return idd;
end

proc A[a:i32]
begin
end

proc B[a, b, c:i32]
begin
end

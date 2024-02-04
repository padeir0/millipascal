proc main
var a
begin
    if Try[10] != 10 begin
        exit 1ss
    end if
    if Try[5] != 5 begin
        exit 2ss
    end if
    if Try[100] != 100 begin
        exit 3ss
    end if
    if Try[33] != 33 begin
        exit 4ss
    end if
end proc

proc Try[n] i64
var idd, i, q, b64
begin
    set idd = 0
    set i = n-1
    while 0 <= i begin
        set idd += 1

        A[q]
        B[b64, q, b64]

        set i -= 1
    end while
    return idd
end proc

proc A[a]
begin
end proc

proc B[a, b, c]
begin
end proc

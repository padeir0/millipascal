proc main
begin
    if F[false, false] != 0 begin
        exit 0ss
    end elseif F[true, true] != 1 begin
        exit 1ss
    end elseif F[false, true] != 2 begin
        exit 2ss
    end elseif F[true, false] != 3 begin
        exit 3ss
    end if
end proc

proc F[a:bool, b:bool] i64
begin
    if not a and not b begin
        return 0
    end elseif a and b begin
        return 1
    end elseif not a and b begin
        return 2
    end elseif a and not b begin
        return 3
    end if
    exit 4ss
end proc

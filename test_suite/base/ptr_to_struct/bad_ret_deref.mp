proc main
begin
    set @F[] = 1ss;
    if @M == 1 begin
        exit 1ss;
    end 
    exit 2ss;
end

data M:ptr^i64 []

proc F[] ptr^i64
begin
    return M;
end

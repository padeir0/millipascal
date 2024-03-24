proc main
begin
    set F[]->O = 1ss;
    if M->O == 1 begin
        exit 1ss;
    end 
    exit 2ss;
end

struct T begin
    O:i64;
end

data M:T []
proc F[] T
begin
    return M;
end

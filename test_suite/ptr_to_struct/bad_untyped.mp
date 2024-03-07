type P is ptr^begin
    [sizeof[i8] + sizeof[i64]]
    tag:i8;
    contents:void;
end

data M:P []

proc main
begin
    set M->contents = 0;
    exit 1ss;
end

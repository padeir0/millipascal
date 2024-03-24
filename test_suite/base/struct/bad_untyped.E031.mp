struct P [sizeof[i8] + sizeof[i64]]
begin
    tag:i8 {0};
    contents:void {P.tag + sizeof[P.tag]};
end

data M:P []

proc main
begin
    set M->contents = 0;
    exit 1ss;
end

struct P begin
    A:i64 {0};
    B:i64 {P.A + sizeof[P.A]};
end

proc main
begin
    exit 1ss;
end

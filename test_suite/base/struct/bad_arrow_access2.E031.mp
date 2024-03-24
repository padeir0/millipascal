struct Node begin
    Value:i64;
    Left, Right:Node;
end

data Forest:Node [1024]

proc main
begin
    set Forest->Value = 1ss; # unmatching types
    exit 1ss;
end

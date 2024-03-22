struct Node begin
    Value:i64;
    Left, Right:Node;
end

data Forest:Node [1024]

proc main
begin
    set Forest->Middle = 1; # invalid name
    exit 1ss;
end

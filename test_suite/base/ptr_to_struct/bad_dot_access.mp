type Node is ptr^begin
    Value:i64;
    Left, Right:Node;
end

data Forest:Node [1024]

proc main
begin
    set Forest.Middle = 1;
    exit 1ss;
end

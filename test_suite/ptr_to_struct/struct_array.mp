type Node is ptr^begin
    Left, Right:Node;
    Value:i64;
end

data Forest:Node [1024]

proc main
var i:i64
begin
    set i = 0;
    while i < sizeof^[Forest] begin
        set Forest.[i]->Value = i;
        set i += 1;
    end
    set i = 0;
    while i < sizeof^[Forest] begin
        if Forest.[i]->Value != i begin
            exit 1ss;
        end
        set i += 1;
    end
end

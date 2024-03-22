struct Node begin
    Left, Right:Node;
    Value:i64;
end

const numTrees = 1024
data Forest:Node [numTrees]

proc main
var i:i64
begin
    set i = 0;
    while i < numTrees begin
        set Forest[i]->Value = i;
        set i += 1;
    end
    set i = 0;
    while i < numTrees begin
        if Forest[i]->Value != i begin
            exit 1ss;
        end
        set i += 1;
    end
end

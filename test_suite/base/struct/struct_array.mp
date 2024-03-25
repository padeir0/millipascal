struct Node begin
    Left, Right:Node;
    Value:i64;
end

const numTrees = 1024l
data Forest:Node [numTrees]

proc main
var i:i64
begin
    set i = 0l;
    while i < numTrees begin
        set Forest[i]->Value = i;
        set i += 1l;
    end
    set i = 0l;
    while i < numTrees begin
        if Forest[i]->Value != i begin
            exit 1ss;
        end
        set i += 1l;
    end
end

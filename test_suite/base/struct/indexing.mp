struct P begin
    X, Y:i32;
end

const numItems = 256
data M:P [numItems]

proc main
var i:i32
begin
    set i = 0;
    do begin
        if (M[i]):ptr != (M:ptr + i * sizeof[P]) begin
            exit 1ss;
        end
        set i += 1;
    end while i < numItems;
end

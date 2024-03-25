from arrays import AI64

const numItems = 32l
data array:AI64 [numItems]

proc main
var i:i64
begin
    set i = 0l;
    while i < numItems begin
        set array[i]->O = i;
        set i += 1l;
    end

    set i = 0l;
    while i < numItems begin
        if array[i]->O != i begin
            exit 1ss;
        end
        set i += 1l;
    end
end

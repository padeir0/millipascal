from arrays import AI32
const numItems = 512l;
data buff:AI32 [numItems]

proc main
var i:i32
begin
    set i = 0l;
    while i < numItems begin
        set (buff[i].O)@i32 = i;
        set i += 1l;
    end
    set i = 0l;
    while i < numItems begin
        if (buff[i].O)@i32 != i begin
            exit 1ss;
        end
        set i += 1l;
    end
end

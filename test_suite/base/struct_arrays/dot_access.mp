from arrays import AI32
const numItems = 512;
data buff:AI32 [numItems]

proc main
var i:i32
begin
    set i = 0;
    while i < numItems begin
        set (buff[i].O)@i32 = i;
        set i += 1;
    end
    set i = 0;
    while i < numItems begin
        if (buff[i].O)@i32 != i begin
            exit 1ss;
        end
        set i += 1;
    end
end

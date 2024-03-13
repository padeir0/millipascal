data buff:ptr^i32 [512]

proc main
var i:i32
begin
    set i = 0l;
    while i:i64 < sizeof^[buff] begin
        set buff.[i]@i32 = i;
        set i += 1l;
    end
    set i = 0l;
    while i:i64 < sizeof^[buff] begin
        if buff.[i]@i32 != i begin
            exit 1ss;
        end
        set i += 1l;
    end
end

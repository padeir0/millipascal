data array:ptr^i64 [32]

proc main
var i:i64
begin
    set i = 0;
    while i < sizeof^[array] begin
        set array->[i] = i;
        set i += 1;
    end

    set i = 0;
    while i < sizeof^[array] begin
        if array->[i] != i begin
            exit 1ss;
        end
        set i += 1;
    end
end

proc main
var i, a,b,c,d,e:i32
begin
    set i = 0;

    set a = 1;
    set b = 2;
    set c = 3;
    set d = 4;
    set e = 5;

    while i < 12
    begin
        set a,b,c,d,e = permute[a,b,c,d,e];
        set i += 1;
    end

    # se permutarmos um número divisivel por 6, voltamos ao original
    if a != 1 or b != 2 or c != 3 or d != 4 or e != 5 begin
        exit 1ss;
    end

end

proc permute[a,b,c,d,e:i32] i32,i32,i32,i32,i32
begin
    return b, a, d, e, c;
end

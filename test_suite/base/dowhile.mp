proc main
var i:i32
begin
    set i = 0;
    do begin
        set i += 1;
    end while i < 10;

    if i != 10 begin
        exit 1ss;
    end

    set i = 0;

    do begin
        set i += 1;
    end while i < 0;

    if i != 1 begin
        exit 1ss;
    end
end

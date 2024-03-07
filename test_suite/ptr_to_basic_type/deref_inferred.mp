
proc main
begin
    test_i64[]
    test_i32[]
    test_i16[]
    test_i8[]
end

data Mi64:ptr^i64 []
proc test_i64
begin
    set @Mi64 = 1;

    if @Mi64 != 1 begin
        exit 1ss;
    end

    set @Mi64 = 2;

    if Mi64@i64 != 2 begin
        exit 2ss;
    end

    set Mi64@i64 = 2;

    if @Mi64 != 2 begin
        exit 3ss;
    end
end

data Mi32:ptr^i32 []
proc test_i32
begin
    set @Mi32 = 1l;
    if @Mi32 != 1l begin
        exit 1ss;
    end

    set @Mi32 = 2l;
    if Mi32@i32 != 2l begin
        exit 2ss;
    end

    set Mi32@i32 = 3l;
    if @Mi32 != 3l begin
        exit 3ss;
    end
end

data Mi16:ptr^i16 []
proc test_i16
begin
    set @Mi16 = 1s;
    if @Mi16 != 1s begin
        exit 1ss;
    end

    set @Mi16 = 2s;
    if Mi16@i16 != 2s begin
        exit 2ss;
    end

    set Mi16@i16 = 3s;
    if @Mi16 != 3s begin
        exit 3ss;
    end
end

data Mi8:ptr^i8 []
proc test_i8
begin
    set @Mi8 = 1ss;
    if @Mi8 != 1ss begin
        exit 1ss;
    end

    set @Mi8 = 2ss;
    if Mi8@i8 != 2ss begin
        exit 2ss;
    end

    set Mi8@i8 = 3ss;
    if @Mi8 != 3ss begin
        exit 3ss;
    end
end

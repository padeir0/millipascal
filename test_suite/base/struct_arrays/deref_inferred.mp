from arrays
import AI64, AI32, AI16, AI8

proc main
begin
    test_i64[];
    test_i32[];
    test_i16[];
    test_i8[];
end

data Mi64:AI64 []
proc test_i64
begin
    set Mi64->O = 1;

    if Mi64->O != 1 begin
        exit 1ss;
    end

    set Mi64->O = 2;

    if (Mi64:ptr)@i64 != 2 begin
        exit 2ss;
    end

    set (Mi64:ptr)@i64 = 3;

    if Mi64->O != 3 begin
        exit 3ss;
    end
end

data Mi32:AI32 []
proc test_i32
begin
    set Mi32->O = 1l;
    if Mi32->O != 1l begin
        exit 1ss;
    end

    set Mi32->O = 2l;
    if (Mi32:ptr)@i32 != 2l begin
        exit 2ss;
    end

    set (Mi32:ptr)@i32 = 3l;
    if Mi32->O != 3l begin
        exit 3ss;
    end
end

data Mi16:AI16 []
proc test_i16
begin
    set Mi16->O = 1s;
    if Mi16->O != 1s begin
        exit 1ss;
    end

    set Mi16->O = 2s;
    if (Mi16:ptr)@i16 != 2s begin
        exit 2ss;
    end

    set (Mi16:ptr)@i16 = 3s;
    if Mi16->O != 3s begin
        exit 3ss;
    end
end

data Mi8:AI8 []
proc test_i8
begin
    set Mi8->O = 1ss;
    if Mi8->O != 1ss begin
        exit 1ss;
    end

    set Mi8->O = 2ss;
    if (Mi8:ptr)@i8 != 2ss begin
        exit 2ss;
    end

    set (Mi8:ptr)@i8 = 3ss;
    if Mi8->O != 3ss begin
        exit 3ss;
    end
end

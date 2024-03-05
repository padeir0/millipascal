proc main
begin
    test_a[];
    test_b[];
    test_c[];
    test_d[];
    test_e[];
end

data a {:i64 0xF, 0xFF, ~0xFFF, ~0xFFFF}

proc test_a
begin
    if (a + 0*sizeof[i64])@i64 != 0xF begin
        exit 1ss;
    end
    if (a + 1*sizeof[i64])@i64 != 0xFF begin
        exit 2ss;
    end
    if (a + 2*sizeof[i64])@i64 != ~0xFFF begin
        exit 3ss;
    end
    if (a + 3*sizeof[i64])@i64 != ~0xFFFF begin
        exit 4ss;
    end

    if sizeof[a] != 4*sizeof[i64] begin
        exit 5ss;
    end
end

data b {:i32 0xAl, ~0xAAl, 0xAAAl}

proc test_b
begin
    if (b + 0*sizeof[i32])@i32 != 0xAl begin
        exit 6ss;
    end
    if (b + 1*sizeof[i32])@i32 != ~0xAAl begin
        exit 7ss;
    end
    if (b + 2*sizeof[i32])@i32 != 0xAAAl begin
        exit 8ss;
    end

    if sizeof[b] != 3*sizeof[i32] begin
        exit 9ss;
    end
end

data c {:i16 0xAs, ~0xAAs, 0xAAAs}

proc test_c
begin
    if (c + 0*sizeof[i16])@i16 != 0xAs begin
        exit 10ss;
    end
    if (c + 1*sizeof[i16])@i16 != ~0xAAs begin
        exit 11ss;
    end
    if (c + 2*sizeof[i16])@i16 != 0xAAAs begin
        exit 12ss;
    end

    if sizeof[c] != 3*sizeof[i16] begin
        exit 13ss;
    end
end

data d {:i8 0xAss, ~0xAss, 0x7Fss}

proc test_d
begin
    if (d + 0*sizeof[i8])@i8 != 0xAss begin
        exit 14ss;
    end
    if (d + 1*sizeof[i8])@i8 != ~0xAss begin
        exit 15ss;
    end
    if (d + 2*sizeof[i8])@i8 != 0x7Fss begin
        exit 16ss;
    end

    if sizeof[d] != 3*sizeof[i8] begin
        exit 17ss;
    end
end

data e {:u8 0xAuss, 0xFuss, 0x7Fuss}

proc test_e
begin
    if (e + 0*sizeof[u8])@u8 != 0xAuss begin
        exit 18ss;
    end
    if (e + 1*sizeof[u8])@u8 != 0xFuss begin
        exit 19ss;
    end
    if (e + 2*sizeof[u8])@u8 != 0x7Fuss begin
        exit 20ss;
    end

    if sizeof[e] != 3*sizeof[u8] begin
        exit 21ss;
    end
end

from utf8
import
    utf8_encode, utf8_decode, MAX_RUNE_SIZE

from buffutil
import
    clear_bytes

from ioutil
import
    put_u8, put_char, put_ln

proc main
begin
    test['A':i32, 'z':i32]
    test[0x3050l, 0x3150l]
    test[0x56E0l, 0x57E0l]
end

data Test [MAX_RUNE_SIZE]

proc test[testRune, upperRune:i32]
var i:i32, p:i64, rune:i32,
    e_size, d_size:i64
begin
    set i = 0l
    set p = 0
    while i < (upperRune-testRune) begin
        clear_bytes[Test, sizeof Test]
        set e_size = utf8_encode[testRune + i, Test]
        write[Test, e_size]

        set rune, d_size = utf8_decode[Test]

        if e_size != d_size begin
            exit 1ss
        end
        if rune != testRune + i begin
            put_u8[Test@u8]
            put_char[' ']
            put_u8[(Test+1)@u8]
            put_char[' ']
            put_u8[(Test+2)@u8]
            put_char[' ']
            put_u8[(Test+3)@u8]
            put_ln[]
            exit 2ss
        end

        set i += 1l
    end
    put_char['\n']
end


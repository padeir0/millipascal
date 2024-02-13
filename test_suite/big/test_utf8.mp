from utf8
import
    utf8_encode, utf8_decode

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
end proc

memory Test 4

proc test[testRune:i32, upperRune:i32]
var i:i32, p:ptr, rune:i32, e_size, d_size
begin
    set i = 0l
    set p = 0p
    while i < (upperRune-testRune) begin
        clear_bytes[Test, Test.size]
        set e_size = utf8_encode[testRune + i, Test]
        write[Test, e_size]

        set rune, d_size = utf8_decode[Test]

        if e_size != d_size begin
            exit 1ss
        end if
        if rune != testRune + i begin
            put_u8[Test@u8]
            put_char[' ']
            put_u8[(Test+1p)@u8]
            put_char[' ']
            put_u8[(Test+2p)@u8]
            put_char[' ']
            put_u8[(Test+3p)@u8]
            put_ln[]
            exit 2ss
        end if

        set i += 1l
    end while
    put_char['\n']
end proc


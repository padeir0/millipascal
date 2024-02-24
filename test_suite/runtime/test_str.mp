from utf8
import
    utf8_decode, utf8_encode, rune_size, MAX_RUNE_SIZE

from ioutil
import
    put_char, put_ln, put_int, put_bool, put_rune

from dfl_alloc
import
    init, validate_init, print_list

from str
import
    new_str, free_str, _free_buff,
    set_str,
    put_str, put_str_debug,
    concat, eq_str, copy_str, dup_str,
    peek_rune, peek, pop_rune, pop,
    hash, length

proc main
begin
    init[]
    test_simple[]
    test_pop_rune['A':i32, 'z':i32]
    test_pop_rune[0x3050l, 0x3150l]
    test_pop_rune[0x56E0l, 0x57E0l]
    test_dup[]
end

data PHRASE0 "My name is Ozymandias"
data PHRASE1 "My name is Ozzy"

proc test_simple
var a:ptr, b:ptr, c:ptr,
    f1:ptr, f2:ptr, f3:ptr, f4:ptr
begin
    set a = new_str[]
    set_str[a, PHRASE0, PHRASE0.size]
    set b = new_str[]
    set_str[b, PHRASE1, PHRASE1.size]

    set f1 = pop[a, 12]
    set f2 = peek[a, 1]
    set f3 = concat[f1, f2]
    set f4 = pop[a, 2]
    set c = concat[f3, f4]

    if not eq_str[b, c] or hash[b] != hash[c] begin
        exit 1ss
    end

    free_str[a]
    free_str[b]

    _free_buff[c] # concat allocates
    free_str[c]

    free_str[f1]
    free_str[f2]

    _free_buff[f3] # concat allocates
    free_str[f3]

    free_str[f4]

    validate_init[]
end

data Buff [64]

proc test_pop_rune[runeStart:i32, runeEnd:i32]
var a:ptr, size, r:i32, i:i32
begin
    set a = new_str[]
    set size = fill_buff[Buff, Buff.size,runeStart, runeEnd]
    set_str[a, Buff, size]

    set r = pop_rune[a]
    set i = 0l
    while r != ~1l begin
        if r != runeStart+i begin
            exit 2ss
        end
        set r = pop_rune[a]
        set i += 1l
    end

    free_str[a]
    validate_init[]
end

proc test_dup
var a:ptr, b:ptr
begin
    set a = new_str[]
    set_str[a, PHRASE0, PHRASE0.size]
    set b = dup_str[a]
    if not eq_str[a, b] or hash[a] != hash[b] begin
        exit 3ss
    end
end

############### UTILITARIES

proc fill_buff[buff:ptr, buffSize, runeStart:i32, runeEnd:i32] i64
var i:i32, p, runeSize
begin
    set i = 0l
    set p = 0
    while i <= (runeEnd-runeStart) and
          p + rune_size[runeStart+i] <= buffSize
    begin
        set runeSize = utf8_encode[runeStart + i, buff+p:ptr]
        set p += runeSize
        set i += 1l
    end
    return p
end

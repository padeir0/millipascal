from bigint import all
from io import fatal
from ioutil import put_ln, put_char, put_int, put_i32, put_bool
from intutil import abs, max
from malloc import set_pool, set_heap, empty
from memsizes import _1KB
from conv import I16MAX, I32MAX
from constants import EQ, MORE, LESS

data begin
    pool1  [1 * _1KB];
    backup [2 * _1KB];
end

proc setup[]
begin
    set_pool[pool1, sizeof[pool1], 0s, sizeof[BigInt]:i16];
    set_heap[backup, sizeof[backup], 0s, I16MAX];
end

data bad "test went wrong!\n"
proc went_wrong[]
begin
    fatal[bad, sizeof[bad]];
    exit 1ss;
end

proc main
begin
    setup[];

    # abs_[a-z]+_digit
    test_abs_add_digit[0, 100, 1];
    test_abs_add_digit_carry[];

    test_abs_sub_digit[0, 100, 1];
    test_abs_sub_digit_carry[];

    test_abs_mult_digit[0, 100, 1];
    test_abs_mult_digit_carry[];

    test_abs_div_digit[1, 100, 1];
    test_abs_div_digit_carry[];

    # abs_[a-z]+
    test_abs_add[0, 100, 1];
    test_abs_add_carry[];

    test_abs_sub[0, 100, 1];
    test_abs_sub_carry[];

    test_abs_mult[0, 100, 1];
    test_abs_mult_carry[];

    test_abs_div[1, 100, 1];
    test_abs_div_carry[];

    # [a-z]+_i32
    test_add_i32[~50, 50, 1];
    test_add_i32_carry[];

    test_sub_i32[~50, 50, 1];
    test_sub_i32_carry[];

    test_mult_i32[~50, 50, 1];
    test_mult_i32_carry[];

    test_div_i32[~50, 50, 1];
    test_div_i32_carry[];

    # full functions
    test_add[~50, 50, 1];
    test_add_carry[];

    test_sub[~50, 50, 1];
    test_sub_carry[];

    test_mult[~50, 50, 1];
    test_mult_carry[];

    test_div[~50, 50, 1];
    test_div_carry[];
end

proc test_abs_add_digit[low, high, pace:i32]
var a, b:BigInt, i, j:i32
begin
    set a = new_BigInt[0];
    set b = new_BigInt[0];

    set i = low;
    while i < high begin
        set j = low;
        while j < high begin
            set_BigInt[a, i];
            abs_add_digit[a, j, b];
            if abs_comp_digit[b, i+j] != EQ begin
                went_wrong[];
            end
            set j += pace;
        end
        set i += pace;
    end

    free_BigInt[a];
    free_BigInt[b];
    if not empty[] begin
        exit 30ss;
    end
end

proc test_abs_add_digit_carry[]
var a, b:BigInt
begin
    set a = new_BigInt[BASE-1];
    set b = new_BigInt[1];
    mult_base[b];
    abs_add_digit[a, 1, a];
    if abs_comp[a, b] != EQ begin
        went_wrong[];
    end

    set_BigInt[a, BASE-1];
    push_digit[a, BASE-1];
    mult_base[b];
    abs_add_digit[a, 1, a];
    if abs_comp[a, b] != EQ begin
        went_wrong[];
    end

    free_BigInt[a];
    free_BigInt[b];
    if not empty[] begin
        exit 32ss;
    end
end

proc test_abs_sub_digit[low, high, pace:i32]
var a, b:BigInt, i, j:i32
begin
    set a = new_BigInt[0];
    set b = new_BigInt[0];

    set i = low;
    while i < high begin
        set j = low;
        while j < high begin
            set_BigInt[a, i];
            abs_sub_digit[a, j, b];
            if abs_comp_digit[b, abs[i-j]] != EQ begin
                went_wrong[];
            end
            set j += pace;
        end
        set i += pace;
    end

    free_BigInt[a];
    free_BigInt[b];
    if not empty[] begin
        exit 31ss;
    end
end

proc test_abs_sub_digit_carry[]
var a, b:BigInt
begin
    set a = new_BigInt[1];
    mult_base[a];
    set b = new_BigInt[BASE-1];
    abs_sub_digit[a, 1, a];
    if abs_comp[a, b] != EQ begin
        went_wrong[];
    end

    set_BigInt[a, 1];
    mult_base[a];
    mult_base[a];

    set_BigInt[b, BASE-1];
    push_digit[b, BASE-1];
    abs_sub_digit[a, 1, a];
    if abs_comp[a, b] != EQ begin
        went_wrong[];
    end

    free_BigInt[a];
    free_BigInt[b];
    if not empty[] begin
        exit 33ss;
    end
end

proc test_abs_mult_digit[low, high, pace:i32]
var a, b:BigInt, i, j:i32
begin
    set a = new_BigInt[0];
    set b = new_BigInt[0];

    set i = low;
    while i < high begin
        set j = low;
        while j < high begin
            set_BigInt[a, i];
            abs_mult_digit[a, j, b];
            if abs_comp_digit[b, i*j] != EQ begin
                went_wrong[];
            end
            set j += pace;
        end
        set i += pace;
    end

    free_BigInt[a];
    free_BigInt[b];
    if not empty[] begin
        exit 34ss;
    end
end

proc test_abs_mult_digit_carry[]
var a, b:BigInt, i:i32
begin
    set a = new_BigInt[1];

    set b = new_BigInt[709_551_616];
    push_digit[b, 446_744_073];
    push_digit[b, 18];
    # b == 18_446_744_073_709_551_616 == 2^64

    set i = 0;
    while i < 64 begin
        abs_mult_digit[a, 2, a];
        set i += 1;
    end

    if abs_comp[a, b] != EQ begin
        went_wrong[];
    end

    free_BigInt[a];
    free_BigInt[b];
    if not empty[] begin
        exit 35ss;
    end
end

proc test_abs_div_digit[low, high, pace:i32]
var a, b:BigInt, i, j:i32
begin
    set a = new_BigInt[0];
    set b = new_BigInt[0];

    set i = low;
    while i < high begin
        set j = low;
        while j < high begin
            set_BigInt[a, i];
            abs_div_digit[a, j, b];
            if abs_comp_digit[b, i/j] != EQ begin
                went_wrong[];
            end
            set j += pace;
        end
        set i += pace;
    end

    free_BigInt[a];
    free_BigInt[b];
    if not empty[] begin
        exit 36ss;
    end
end

proc test_abs_div_digit_carry[]
var a, b:BigInt, i:i32
begin
    # a == 18_446_744_073_709_551_616 == 2^64
    set a = new_BigInt[709_551_616];
    push_digit[a, 446_744_073];
    push_digit[a, 18];
    set b = new_BigInt[1];

    set i = 0;
    while i < 64 begin
        abs_div_digit[a, 2, b];
        copy_BigInt[a, b];
        set i += 1;
    end

    if abs_comp_digit[a, 1] != EQ begin
        put_BigInt[a];
        went_wrong[];
    end

    free_BigInt[a];
    free_BigInt[b];
    if not empty[] begin
        exit 37ss;
    end
end

proc test_abs_add[low, high, pace:i32]
var a, b:BigInt, i, j:i32
begin
    set a = new_BigInt[0];
    set b = new_BigInt[0];

    set i = low;
    while i < high begin
        set j = low;
        while j < high begin
            set_BigInt[a, i];
            set_BigInt[b, j];
            abs_add[a, b, a];
            if abs_comp_digit[a, i+j] != EQ begin
                went_wrong[];
            end
            set j += pace;
        end
        set i += pace;
    end

    free_BigInt[a];
    free_BigInt[b];
    if not empty[] begin
        exit 38ss;
    end
end

proc test_abs_add_carry[]
var a, b, c:BigInt
begin
    set a = new_BigInt[BASE-1];
    set b = new_BigInt[1];
    set c = new_BigInt[1];
    mult_base[c];
    abs_add[a, b, a];
    if abs_comp[a, c] != EQ begin
        went_wrong[];
    end

    set_BigInt[a, BASE-1];
    push_digit[a, BASE-1];
    mult_base[c];
    abs_add[a, b, a];
    if abs_comp[a, c] != EQ begin
        went_wrong[];
    end

    free_BigInt[a];
    free_BigInt[b];
    free_BigInt[c];
    if not empty[] begin
        exit 39ss;
    end
end

proc test_abs_sub[low, high, pace:i32]
var a, b:BigInt, i, j:i32
begin
    set a = new_BigInt[0];
    set b = new_BigInt[0];

    set i = low;
    while i < high begin
        set j = low;
        while j < high begin
            set_BigInt[a, i];
            set_BigInt[b, j];
            abs_sub[a, b, a];
            if abs_comp_digit[a, abs[i-j]] != EQ begin
                went_wrong[];
            end
            set j += pace;
        end
        set i += pace;
    end

    free_BigInt[a];
    free_BigInt[b];
    if not empty[] begin
        exit 40ss;
    end
end

proc test_abs_sub_carry[]
var a, b, c:BigInt
begin
    set a = new_BigInt[1];
    mult_base[a];
    set b = new_BigInt[1];
    set c = new_BigInt[BASE-1];
    abs_sub[a, b, a];
    if abs_comp[a, c] != EQ begin
        went_wrong[];
    end

    set_BigInt[a, 1];
    mult_base[a];
    mult_base[a];
    set_BigInt[c, BASE-1];
    push_digit[c, BASE-1];
    abs_sub[a, b, a];
    if abs_comp[a, c] != EQ begin
        went_wrong[];
    end

    free_BigInt[a];
    free_BigInt[b];
    free_BigInt[c];
    if not empty[] begin
        exit 41ss;
    end
end

proc test_abs_mult[low, high, pace:i32]
var a, b, c:BigInt, i, j:i32
begin
    set a = new_BigInt[0];
    set b = new_BigInt[0];
    set c = new_BigInt[0];

    set i = low;
    while i < high begin
        set j = low;
        while j < high begin
            set_BigInt[a, i];
            set_BigInt[b, j];
            abs_mult[a, b, c];
            if abs_comp_digit[c, i*j] != EQ begin
                went_wrong[];
            end
            set j += pace;
        end
        set i += pace;
    end

    free_BigInt[a];
    free_BigInt[b];
    free_BigInt[c];
    if not empty[] begin
        exit 42ss;
    end
end

proc test_abs_mult_carry[]
var a, b, c, d:BigInt, i:i32
begin
    set a = new_BigInt[1];
    set b = new_BigInt[2];
    set c = new_BigInt[0];

    set d = new_BigInt[709_551_616];
    push_digit[d, 446_744_073];
    push_digit[d, 18];
    # b == 18__446_744_073__709_551_616 == 2^64

    set i = 0;
    while i < 64 begin
        abs_mult[a, b, c];
        copy_BigInt[a, c];
        set i += 1;
    end

    if abs_comp[a, d] != EQ begin
        went_wrong[];
    end

    free_BigInt[a];
    free_BigInt[b];
    free_BigInt[c];
    free_BigInt[d];
    if not empty[] begin
        exit 43ss;
    end
end

proc test_abs_div[low, high, pace:i32]
var a, b, q, r:BigInt, i, j:i32
begin
    set a = new_BigInt[0];
    set b = new_BigInt[0];
    set q = new_BigInt[0];
    set r = new_BigInt[0];

    set i = low;
    while i < high begin
        set j = low;
        while j < high begin
            set_BigInt[a, i];
            set_BigInt[b, j];
            abs_div[a, b, q, r];
            if abs_comp_digit[q, i/j] != EQ or
               abs_comp_digit[r, i%j] != EQ
            begin
                went_wrong[];
            end
            set j += pace;
        end
        set i += pace;
    end

    free_BigInt[a];
    free_BigInt[b];
    free_BigInt[q];
    free_BigInt[r];
    if not empty[] begin
        exit 44ss;
    end
end

proc test_abs_div_carry[]
var a,b,q,r, res:BigInt, i:i32
begin
    # a == 18__446_744_073__709_551_616 == 2^64
    set a = new_BigInt[709_551_616];
    push_digit[a, 446_744_073];
    push_digit[a, 18];
    set b = new_BigInt[2];
    set q = new_BigInt[0];
    set r = new_BigInt[0];

    set res = new_BigInt[1];

    set i = 0;
    while i < 64 begin
        abs_div[a, b, q, r];
        if abs_comp_digit[r, 0] != EQ begin
            went_wrong[];
        end
        copy_BigInt[a, q];
        set i += 1;
    end

    if abs_comp[a, res] != EQ begin
        went_wrong[];
    end

    free_BigInt[a];
    free_BigInt[b];
    free_BigInt[q];
    free_BigInt[r];
    free_BigInt[res];
    if not empty[] begin
        exit 45ss;
    end
end

########################### INTEGER ARITHMETIC

proc test_add_i32[low, high, pace:i32]
var a, b:BigInt, i, j:i32
begin
    set a = new_BigInt[0];
    set b = new_BigInt[0];

    set i = low;
    while i < high begin
        set j = low;
        while j < high begin
            set_BigInt[a, i];
            add_i32[a, j, b];
            if comp_i32[b, i+j] != EQ begin
                went_wrong[];
            end
            set j += pace;
        end
        set i += pace;
    end

    free_BigInt[a];
    free_BigInt[b];
    if not empty[] begin
        exit 46ss;
    end
end

proc test_add_i32_carry[]
var a, b:BigInt
begin
    set a = new_BigInt[BASE-1];
    set b = new_BigInt[1];
    mult_base[b];
    add_i32[a, 1, a];
    if comp[a, b] != EQ begin
        went_wrong[];
    end
    add_i32[a, ~1, a];
    if comp_i32[a, BASE-1] != EQ begin
        went_wrong[];
    end

    set_BigInt[a, BASE-1];
    push_digit[a, BASE-1];
    mult_base[b];
    add_i32[a, 1, a];
    if comp[a, b] != EQ begin
        went_wrong[];
    end

    free_BigInt[a];
    free_BigInt[b];
    if not empty[] begin
        exit 47ss;
    end
end

proc test_sub_i32[low, high, pace:i32]
var a, b:BigInt, i, j:i32
begin
    set a = new_BigInt[0];
    set b = new_BigInt[0];

    set i = low;
    while i < high begin
        set j = low;
        while j < high begin
            set_BigInt[a, i];
            sub_i32[a, j, b];
            if comp_i32[b, i-j] != EQ begin
                went_wrong[];
            end
            set j += pace;
        end
        set i += pace;
    end

    free_BigInt[a];
    free_BigInt[b];
    if not empty[] begin
        exit 48ss;
    end
end

proc test_sub_i32_carry[]
var a, b, base:BigInt
begin
    set base = new_BigInt[1]; mult_base[base];

    set a = new_BigInt[1]; copy_BigInt[a, base];
    set b = new_BigInt[BASE-1];

    sub_i32[a, 1, a];
    if comp[a, b] != EQ begin
        went_wrong[];
    end
    sub_i32[a, ~1, a];
    if comp[a, base] != EQ begin
        went_wrong[];
    end

    set_BigInt[a, 1];
    mult_base[a];
    mult_base[a];

    set_BigInt[b, BASE-1];
    push_digit[b, BASE-1];
    sub_i32[a, 1, a];
    if comp[a, b] != EQ begin
        went_wrong[];
    end

    free_BigInt[a];
    free_BigInt[b];
    free_BigInt[base];
    if not empty[] begin
        exit 49ss;
    end
end

proc test_mult_i32[low, high, pace:i32]
var a, b:BigInt, i, j:i32
begin
    set a = new_BigInt[0];
    set b = new_BigInt[0];

    set i = low;
    while i < high begin
        set j = low;
        while j < high begin
            set_BigInt[a, i];
            mult_i32[a, j, b];
            if comp_i32[b, i*j] != EQ begin
                went_wrong[];
            end
            set j += pace;
        end
        set i += pace;
    end

    free_BigInt[a];
    free_BigInt[b];
    if not empty[] begin
        exit 50ss;
    end
end

proc test_mult_i32_carry[]
var a, b:BigInt, i:i32
begin
    set a = new_BigInt[1];

    set b = new_BigInt[709_551_616];
    push_digit[b, 446_744_073];
    push_digit[b, 18];
    # b == 18_446_744_073_709_551_616 == 2^64

    set i = 0;
    while i < 64 begin
        mult_i32[a, ~2, a];
        set i += 1;
    end

    if comp[a, b] != EQ begin
        went_wrong[];
    end

    free_BigInt[a];
    free_BigInt[b];
    if not empty[] begin
        exit 51ss;
    end
end

proc test_div_i32[low, high, pace:i32]
var a, b:BigInt, i, j:i32
begin
    set a = new_BigInt[0];
    set b = new_BigInt[0];

    set i = low;
    while i < high begin
        set j = low;
        while j < high begin
            if j != 0 begin
                set_BigInt[a, i];
                div_i32[a, j, b];
                if comp_i32[b, i/j] != EQ begin
                    put_BigInt[b];
                    put_char[' '];
                    put_i32[i/j];
                    put_ln[];
                    went_wrong[];
                end
            end
            set j += pace;
        end
        set i += pace;
    end

    free_BigInt[a];
    free_BigInt[b];
    if not empty[] begin
        exit 52ss;
    end
end

proc test_div_i32_carry[]
var a, b:BigInt, i:i32
begin
    # a == 18_446_744_073_709_551_616 == 2^64
    set a = new_BigInt[709_551_616];
    push_digit[a, 446_744_073];
    push_digit[a, 18];

    set b = new_BigInt[1];

    set i = 0;
    while i < 64 begin
        div_i32[a, ~2, b];
        copy_BigInt[a, b];
        set i += 1;
    end

    if comp_i32[a, 1] != EQ begin
        went_wrong[];
    end

    free_BigInt[a];
    free_BigInt[b];
    if not empty[] begin
        exit 53ss;
    end
end

proc test_add[low, high, pace:i32]
var a, b:BigInt, i, j:i32
begin
    set a = new_BigInt[0];
    set b = new_BigInt[0];

    set i = low;
    while i < high begin
        set j = low;
        while j < high begin
            set_BigInt[a, i];
            set_BigInt[b, j];
            add[a, b, a];
            if comp_i32[a, i+j] != EQ begin
                went_wrong[];
            end
            set j += pace;
        end
        set i += pace;
    end

    free_BigInt[a];
    free_BigInt[b];
    if not empty[] begin
        exit 54ss;
    end
end

proc test_add_carry[]
var a, b, c:BigInt
begin
    set a = new_BigInt[BASE-1];
    set b = new_BigInt[1];
    set c = new_BigInt[1];
    mult_base[c];
    add[a, b, a];
    if comp[a, c] != EQ begin
        went_wrong[];
    end

    set_BigInt[a, BASE-1];
    push_digit[a, BASE-1];
    mult_base[c];
    add[a, b, a];
    if comp[a, c] != EQ begin
        went_wrong[];
    end

    free_BigInt[a];
    free_BigInt[b];
    free_BigInt[c];
    if not empty[] begin
        exit 55ss;
    end
end

proc test_sub[low, high, pace:i32]
var a, b:BigInt, i, j:i32
begin
    set a = new_BigInt[0];
    set b = new_BigInt[0];

    set i = low;
    while i < high begin
        set j = low;
        while j < high begin
            set_BigInt[a, i];
            set_BigInt[b, j];
            sub[a, b, a];
            if comp_i32[a, i-j] != EQ begin
                went_wrong[];
            end
            set j += pace;
        end
        set i += pace;
    end

    free_BigInt[a];
    free_BigInt[b];
    if not empty[] begin
        exit 56ss;
    end
end

proc test_sub_carry[]
var a, b, c:BigInt
begin
    set a = new_BigInt[1];
    mult_base[a];
    set b = new_BigInt[1];
    set c = new_BigInt[BASE-1];
    sub[a, b, a];
    if comp[a, c] != EQ begin
        went_wrong[];
    end

    set_BigInt[a, 1];
    mult_base[a];
    mult_base[a];
    set_BigInt[c, BASE-1];
    push_digit[c, BASE-1];
    sub[a, b, a];
    if comp[a, c] != EQ begin
        went_wrong[];
    end

    free_BigInt[a];
    free_BigInt[b];
    free_BigInt[c];
    if not empty[] begin
        exit 57ss;
    end
end

proc test_mult[low, high, pace:i32]
var a, b, c:BigInt, i, j:i32
begin
    set a = new_BigInt[0];
    set b = new_BigInt[0];
    set c = new_BigInt[0];

    set i = low;
    while i < high begin
        set j = low;
        while j < high begin
            set_BigInt[a, i];
            set_BigInt[b, j];
            mult[a, b, c];
            if comp_i32[c, i*j] != EQ begin
                went_wrong[];
            end
            set j += pace;
        end
        set i += pace;
    end

    free_BigInt[a];
    free_BigInt[b];
    free_BigInt[c];
    if not empty[] begin
        exit 58ss;
    end
end

proc test_mult_carry[]
var a, b, c, d:BigInt, i:i32
begin
    set a = new_BigInt[1];
    set b = new_BigInt[~2];
    set c = new_BigInt[0];

    set d = new_BigInt[709_551_616];
    push_digit[d, 446_744_073];
    push_digit[d, 18];
    # b == 18__446_744_073__709_551_616 == (~2)^64

    set i = 0;
    while i < 64 begin
        mult[a, b, c];
        copy_BigInt[a, c];
        set i += 1;
    end

    if comp[a, d] != EQ begin
        went_wrong[];
    end

    free_BigInt[a];
    free_BigInt[b];
    free_BigInt[c];
    free_BigInt[d];
    if not empty[] begin
        exit 59ss;
    end
end

proc test_div[low, high, pace:i32]
var a, b, q, r:BigInt, i, j:i32
begin
    set a = new_BigInt[0];
    set b = new_BigInt[0];
    set q = new_BigInt[0];
    set r = new_BigInt[0];

    set i = low;
    while i < high begin
        set j = low;
        while j < high begin
            if j != 0 begin
                set_BigInt[a, i];
                set_BigInt[b, j];
                div[a, b, q, r];
                if comp_i32[q, i/j] != EQ or
                   comp_i32[r, i%j] != EQ
                begin
                    went_wrong[];
                end
            end
            set j += pace;
        end
        set i += pace;
    end

    free_BigInt[a];
    free_BigInt[b];
    free_BigInt[q];
    free_BigInt[r];
    if not empty[] begin
        exit 60ss;
    end
end

proc test_div_carry[]
var a,b,q,r, res:BigInt, i:i32
begin
    # a == 18__446_744_073__709_551_616 == 2^64
    set a = new_BigInt[709_551_616];
    push_digit[a, 446_744_073];
    push_digit[a, 18];
    set b = new_BigInt[~2];
    set q = new_BigInt[0];
    set r = new_BigInt[0];

    set res = new_BigInt[1];

    set i = 0;
    while i < 64 begin
        div[a, b, q, r];
        if comp_i32[r, 0] != EQ begin
            went_wrong[];
        end
        copy_BigInt[a, q];
        set i += 1;
    end

    if comp[a, res] != EQ begin
        went_wrong[];
    end

    free_BigInt[a];
    free_BigInt[b];
    free_BigInt[q];
    free_BigInt[r];
    free_BigInt[res];
    if not empty[] begin
        exit 61ss;
    end
end

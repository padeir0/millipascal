# divides two numbers x, y by doing binary search
# ie. by finding a quocient q such that x = y*b + r
# where r is the remainder
#
# it is a theorem that whatever this q may be, it obeys:
#     q*y <= x
#     (q+1)*y > x

from ioutil import put_int, put_char

proc main
var x, y, q,
begin
    set x = 1
    while x < HIGHEST begin
        set y = 1
        while y < HIGHEST begin
            set q = Search[x, y]
            if q != x/y begin
                put_int[x]
                put_char['/']
                put_int[y]
                put_char['\n']
                exit 1ss
            end if
            set y += 1
        end while
        set x += 1
    end while
end proc

const EQ 0
const HIGH 1
const LOW 2

const LOWEST 0
const HIGHEST 2048
const MAXITER 12 # binary search has O(log_2) so log_2 (2048) = 11

# finds x/y
proc Search[x, y] i64
var high, low, guess, res, i
begin
    set low = LOWEST
    set high = HIGHEST
    set guess = (low+high)>>1 # divide by 2 using right shifts

    set i = 0
    set res = Test[x, y, guess]
    while res != EQ and i < MAXITER begin
        if res == HIGH begin
            set high = guess
        end elseif res == LOW begin
            set low = guess
        end if
        set guess = (low+high)>>1
        set res = Test[x, y, guess]
        set i += 1
    end while

    return guess
end proc

memory unreachable "unreachable!\n"

proc Test[x, y, guess] i64
begin
    if (guess*y == x) or
       (guess*y < x and (guess+1)*y > x)
    begin
        return EQ
    end if
    if guess*y > x begin
        return HIGH
    end if
    if guess*y < x begin
        return LOW
    end if

    write[unreachable, unreachable.size]
    exit 2ss
end proc


memory H "high\n"
memory L "low\n"
memory E "eq\n"

proc put_res[res]
begin
    if res == EQ begin
        write[E, E.size]
        return
    end elseif res == HIGH begin
        write[H, H.size]
        return
    end elseif res == LOW begin
        write[L, L.size]
        return
    end if

    write[unreachable, unreachable.size]
    exit 3ss
end proc

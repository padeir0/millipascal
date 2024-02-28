# divides two numbers x, y by doing binary search
# ie. by finding a quocient q such that x = y*b + r
# where r is the remainder
#
# it is a theorem that whatever this q may be, it obeys:
#     q*y <= x
#     (q+1)*y > x

proc main
var x, y, q:i64,
begin
    set x = 1
    while x < HIGHEST begin
        set y = 1
        while y < HIGHEST begin
            set q = Search[x, y]
            if q != x/y begin
                exit 1ss
            end
            set y += 1
        end
        set x += 1
    end
end

const EQ = 0
const HIGH = 1
const LOW = 2

const LOWEST = 0
const HIGHEST = 2048
const MAXITER = 12 # binary search has O(log_2) so log_2 (2048) = 11

# finds x/y
proc Search[x, y:i64] i64
var high, low, guess, res, i:i64
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
        end
        set guess = (low+high)>>1
        set res = Test[x, y, guess]
        set i += 1
    end

    return guess
end

data unreachable "unreachable!\n"

proc Test[x, y, guess:i64] i64
begin
    if (guess*y == x) or
       (guess*y < x and (guess+1)*y > x)
    begin
        return EQ
    end
    if guess*y > x begin
        return HIGH
    end
    if guess*y < x begin
        return LOW
    end

    write[unreachable, unreachable.size]
    exit 2ss
end


data H "high\n"
data L "low\n"
data E "eq\n"

proc put_res[res:i64]
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
    end

    write[unreachable, unreachable.size]
    exit 3ss
end

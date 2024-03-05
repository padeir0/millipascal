data a [32 * sizeof[i32]]
data b [32 * sizeof[i64]]
data c [32 * sizeof[ptr]]

data heap [1 << 16]
data bytemap [sizeof[heap]/sizeof[ptr]]

const u8max = (1<<8)-1
data myBuff [u8max]

proc main
begin
    if sizeof[a] != 32 * sizeof[i32] begin
        exit 1ss;
    end
    if sizeof[b] != 32 * sizeof[i64] begin
        exit 2ss;
    end
    if sizeof[c] != 32 * sizeof[ptr] begin
        exit 3ss;
    end
    if sizeof[bytemap] != (1<<16)/sizeof[ptr] begin
        exit 4ss;
    end
    if sizeof[myBuff] != 255 begin
        exit 5ss;
    end
end

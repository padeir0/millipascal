data a [32 * sizeof i32]
data b [32 * sizeof i64]
data c [32 * sizeof ptr]

data heap [1 << 16]
data bytemap [heap.size/8]

const u8max = (1<<8)-1
data myBuff [u8max]

proc main
begin
    if a.size != 32 * sizeof i32 begin
        exit 1ss
    end if
    if b.size != 32 * sizeof i64 begin
        exit 2ss
    end if
    if c.size != 32 * sizeof ptr begin
        exit 3ss
    end if
    if bytemap.size != (1<<16)/8 begin
        exit 4ss
    end if
    if myBuff.size != 255 begin
        exit 5ss
    end if
end proc

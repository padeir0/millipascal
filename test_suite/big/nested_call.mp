proc main
var a:i32, b:i32, neg:bool
begin
    set a = 5l
    set b = ~1l
    set neg = mult_signal[is_neg[a], is_neg[b]]
    if not neg begin
        exit 2ss
    end if
end proc

proc is_neg[a:i32] bool
begin
    return a < 0l
end proc

proc mult_signal[a:bool, b:bool] bool
begin
    # 1*1 = (-1)*(-1) = 1
    if (not a and not b) or (a and b) begin
        return false
    # (-1)*1 = 1*(-1) = -1
    end elseif (not a and b) or (a and not b) begin
        return true
    end if
    exit 1ss
end proc

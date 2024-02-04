proc main
var a:i32, b:i32, c:i32, d:i32,
    e:i32, f:i32, g:i32, h:i32,
    neg:bool
begin
    set a = 5l
    set b = ~1l
    set c = ~1l
    set d = 10l

    set e = 5l
    set f = ~1l
    set g = ~1l
    set h = ~10l
    
    set neg = mult_signal[is_neg[a], is_neg[b]]
    if not neg begin
        exit 2ss
    end if
    set neg = mult_4signals[is_neg[a], is_neg[b], is_neg[c], is_neg[d]]
    if neg begin
        exit 3ss
    end if
    set neg = mult_8signals[
        is_neg[a], is_neg[b], is_neg[c], is_neg[d],
        is_neg[e], is_neg[f], is_neg[g], is_neg[h]
    ]
    if not neg begin
        exit 4ss
    end if

    set neg = mult_16signals[
        is_neg[a], is_neg[b], is_neg[c], is_neg[d],
        is_neg[e], is_neg[f], is_neg[g], is_neg[h],
        is_neg[a], is_neg[b], is_neg[c], is_neg[d],
        is_neg[e], is_neg[f], is_neg[g], is_neg[h]
    ]
    if neg begin
        exit 5ss
    end if

    set neg = mult_16signals[
        is_neg[a], is_neg[b], is_neg[c], is_neg[d],
        is_neg[e], is_neg[f], is_neg[g], is_neg[h],
        is_neg[e], is_neg[f], is_neg[g], is_neg[h],
        is_neg[e], is_neg[f], is_neg[g], is_neg[h]
    ]
    if not neg begin
        exit 6ss
    end if

    set neg = mult_16signals[
        is_neg[a], is_neg[b], is_neg[c], is_neg[d],
        is_neg[a], is_neg[b], is_neg[c], is_neg[d],
        is_neg[a], is_neg[b], is_neg[c], is_neg[d],
        is_neg[a], is_neg[b], is_neg[c], is_neg[d]
    ]
    if neg begin
        exit 7ss
    end if

    set neg = mult_16signals[
        is_neg[e], is_neg[f], is_neg[g], is_neg[h],
        is_neg[e], is_neg[f], is_neg[g], is_neg[h],
        is_neg[a], is_neg[b], is_neg[c], is_neg[d],
        is_neg[e], is_neg[f], is_neg[g], is_neg[h]
    ]
    if not neg begin
        exit 8ss
    end if
end proc

proc is_neg[a:i32] bool
begin
    return a < 0l
end proc

proc mult_4signals[a:bool, b:bool, c:bool, d:bool] bool
begin
    return mult_signal[mult_signal[a, b], mult_signal[c, d]]
end proc

proc mult_8signals[
    a:bool, b:bool, c:bool, d:bool,
    e:bool, f:bool, g:bool, h:bool
] bool
begin
    return mult_signal[
            mult_4signals[a, b, c, d],
            mult_4signals[e, f, g, h]
    ]
end proc

proc mult_16signals[
    a:bool, b:bool, c:bool, d:bool,
    e:bool, f:bool, g:bool, h:bool,
    i:bool, j:bool, k:bool, l:bool,
    m:bool, n:bool, o:bool, p:bool,
] bool
begin
    return mult_signal[
            mult_8signals[a, b, c, d, e, f, g, h],
            mult_8signals[i, j, k, l, m, n, o, p]
    ]
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

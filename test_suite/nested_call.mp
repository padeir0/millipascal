proc main
var a, b, c, d:i32,
    e, f, g, h:i32,
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
    end
    set neg = mult_4signals[is_neg[a], is_neg[b], is_neg[c], is_neg[d]]
    if neg begin
        exit 3ss
    end
    set neg = mult_8signals[
        is_neg[a], is_neg[b], is_neg[c], is_neg[d],
        is_neg[e], is_neg[f], is_neg[g], is_neg[h]
    ]
    if not neg begin
        exit 4ss
    end

    set neg = mult_16signals[
        is_neg[a], is_neg[b], is_neg[c], is_neg[d],
        is_neg[e], is_neg[f], is_neg[g], is_neg[h],
        is_neg[a], is_neg[b], is_neg[c], is_neg[d],
        is_neg[e], is_neg[f], is_neg[g], is_neg[h]
    ]
    if neg begin
        exit 5ss
    end

    set neg = mult_16signals[
        is_neg[a], is_neg[b], is_neg[c], is_neg[d],
        is_neg[e], is_neg[f], is_neg[g], is_neg[h],
        is_neg[e], is_neg[f], is_neg[g], is_neg[h],
        is_neg[e], is_neg[f], is_neg[g], is_neg[h]
    ]
    if not neg begin
        exit 6ss
    end

    set neg = mult_16signals[
        is_neg[a], is_neg[b], is_neg[c], is_neg[d],
        is_neg[a], is_neg[b], is_neg[c], is_neg[d],
        is_neg[a], is_neg[b], is_neg[c], is_neg[d],
        is_neg[a], is_neg[b], is_neg[c], is_neg[d]
    ]
    if neg begin
        exit 7ss
    end

    set neg = mult_16signals[
        is_neg[e], is_neg[f], is_neg[g], is_neg[h],
        is_neg[e], is_neg[f], is_neg[g], is_neg[h],
        is_neg[a], is_neg[b], is_neg[c], is_neg[d],
        is_neg[e], is_neg[f], is_neg[g], is_neg[h]
    ]
    if not neg begin
        exit 8ss
    end
end

proc is_neg[a:i32] bool
begin
    return a < 0l
end

proc mult_4signals[a, b, c, d:bool] bool
begin
    return mult_signal[mult_signal[a, b], mult_signal[c, d]]
end

proc mult_8signals[
    a, b, c, d:bool,
    e, f, g, h:bool
] bool
begin
    return mult_signal[
            mult_4signals[a, b, c, d],
            mult_4signals[e, f, g, h]
    ]
end

proc mult_16signals[
    a, b, c, d:bool,
    e, f, g, h:bool,
    i, j, k, l:bool,
    m, n, o, p:bool,
] bool
begin
    return mult_signal[
            mult_8signals[a, b, c, d, e, f, g, h],
            mult_8signals[i, j, k, l, m, n, o, p]
    ]
end

proc mult_signal[a, b:bool] bool
begin
    # 1*1 = (-1)*(-1) = 1
    if (not a and not b) or (a and b) begin
        return false
    # (-1)*1 = 1*(-1) = -1
    end elseif (not a and b) or (a and not b) begin
        return true
    end
    exit 1ss
end

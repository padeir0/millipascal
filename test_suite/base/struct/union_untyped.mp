struct Base [sizeof[i8] + sizeof[i64]]
begin
    tag:i8        {0};
    contents:void {Base.tag+sizeof[Base.tag]};
end

const begin
    SIGNED = 0ss;
    UNSIGNED = 1ss;
end

data M:Base []

proc main
begin
    set M->tag = SIGNED;
    set (M.contents)@i64 = ~1l;
    verify[M];
    set M->tag = UNSIGNED;
    set (M.contents)@u64 = 1ul;
    verify[M];
end

proc verify[p:Base]
begin
    if M->tag == SIGNED begin
        if (M.contents)@i64 != ~1l begin
            exit 1ss;
        end
    end elseif M->tag == UNSIGNED begin
        if (M.contents)@u64 != 1ul begin
            exit 2ss;
        end
    end else begin
        exit 3ss;
    end
end

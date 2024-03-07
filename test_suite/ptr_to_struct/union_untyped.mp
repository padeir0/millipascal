type Base is ptr^begin
    [sizeof[i8] + sizeof[i64]]
    tag:i8        {0};
    contents:void {tag+sizeof[tag]}
end

const begin
    SIGNED = 0ss;
    UNSIGNED = 1ss;
end

data M:Base []

proc main
begin
    set M->tag = SIGNED;
    set (M.contents)@i64 = ~1;
    verify[M];
    set M->tag = UNSIGNED;
    set (M.contents)@u64 = 1;
    verify[M];
end

proc verify[p:Base]
begin
    if M->tag == SIGNED begin
        if (M.contents)@i64 != ~1 begin
            exit 1ss;
        end
    end elseif M->tag == UNSIGNED begin
        if (M.unsigned)@i64 != 1 begin
            exit 2ss;
        end
    end else begin
        exit 3ss;
    end
end

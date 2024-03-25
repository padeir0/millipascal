struct Base [sizeof[i8] + sizeof[i64]]
begin
    tag:i8       {0};
    signed:i64   {Base.tag+sizeof[Base.tag]};
    unsigned:u64 {Base.signed};
end

const begin
    SIGNED = 0ss;
    UNSIGNED = 1ss;
end

data M:Base []

proc main
begin
    set M->tag = SIGNED;
    set M->signed = ~1l;
    verify[M];
    set M->tag = UNSIGNED;
    set M->unsigned = 1ul;
    verify[M];
end

proc verify[p:Base]
begin
    if M->tag == SIGNED begin
        if M->signed != ~1l begin
            exit 1ss;
        end
    end elseif M->tag == UNSIGNED begin
        if M->unsigned != 1ul begin
            exit 2ss;
        end
    end else begin
        exit 3ss;
    end
end

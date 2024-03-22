struct P [0] begin
    X:void {0};
end

proc main
begin
    if sizeof[P.X] == 1 begin
        exit 1ss;
    end
    exit 2ss;
end

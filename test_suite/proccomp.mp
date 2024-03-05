proc main
var a:proc[][]
begin
    set a = F;

    if a != F begin
        exit 1ss;
    end

    if not(a == F) begin
        exit 2ss;
    end

    if a == G begin
        exit 3ss;
    end

    if not (a != G) begin
        exit 4ss;
    end

    if G == F begin
        exit 5ss;
    end
end

proc F[]
begin
end

proc G[]
begin
end

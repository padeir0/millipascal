proc main
var a:proc[][]
begin
    set a = F

    if a != F begin
        exit 1ss
    end if

    if not(a == F) begin
        exit 2ss
    end if

    if a == G begin
        exit 3ss
    end if

    if not (a != G) begin
        exit 4ss
    end if

    if G == F begin
        exit 5ss
    end if
end proc

proc F[]
begin
end proc

proc G[]
begin
end proc

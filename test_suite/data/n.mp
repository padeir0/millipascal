import m

const a = sizeof[m::M]

proc main
begin
    if sizeof[m::M] != 100 begin
        exit 1ss;
    end
    if a != 100 begin
        exit 2ss;
    end 
end

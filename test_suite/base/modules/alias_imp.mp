import export_all as nums

proc main
begin
    if nums::TWO != 2*nums::ONE begin
        exit 1ss;
    end
    if nums::THREE != 3*nums::ONE begin
        exit 2ss;
    end
end

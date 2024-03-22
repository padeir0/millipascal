struct List begin
    Value:i64;
    Next:List;
end

data Head:List []

proc main
begin
    set Head->Value = Head;
    set Head->Next = 1;
    exit 1ss;
end

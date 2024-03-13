type List is ptr^begin
    Value:i64;
    Next:List;
end

data Head:List []

proc main
begin
    set Head->Value, Head->Next = F[];
    if Head->Value != 1 begin
        exit 1ss;
    end
    if Head->Next != Head begin
        exit 2ss;
    end
end

proc F[] i64, List
begin
    return 1, Head;
end

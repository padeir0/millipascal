struct Window [sizeof[i64]]
begin
    Prev:i64 {~sizeof[i64]};
    Curr:i64 {0};
    Next:i64 {sizeof[i64]};
end

const numItems = 64
data M:Window [numItems]

const last = numItems-1
proc main
var i:i64
begin
    set i = 1;
    while i < last begin
        set M[i]->Prev = i;
        set i += 1;
    end
    set i = 1;
    while i < last begin
        if M[i]->Prev != i begin
            exit 1ss;
        end
        set i += 1;
    end

    set i = 1;
    while i < last begin
        set M[i]->Curr = i;
        set i += 1;
    end
    set i = 1;
    while i < last begin
        if M[i]->Curr != i begin
            exit 2ss;
        end
        set i += 1;
    end

    set i = 1;
    while i < last begin
        set M[i]->Next = i;
        set i += 1;
    end
    set i = 1;
    while i < last begin
        if M[i]->Next != i begin
            exit 3ss;
        end
        set i += 1;
    end
end

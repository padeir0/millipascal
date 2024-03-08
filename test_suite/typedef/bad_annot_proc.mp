type Binary as proc[i64, i64][i64]

proc Add:Binary [a]
begin
    return a+b;
end

proc Mult:Binary [a, b]
begin
    return a*b;
end

proc main
begin
    if Mult[Add[1, 1], 2] != 4 begin
        exit 1ss;
    end
end

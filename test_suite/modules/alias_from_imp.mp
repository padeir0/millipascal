from
    export_all
import
    ONE   as n_1,
    TWO   as n_2,
    THREE as n_3

proc main
begin
    if n_2 != 2*n_1 begin
        exit 1ss;
    end
    if n_3 != 3*n_1 begin
        exit 2ss;
    end
end

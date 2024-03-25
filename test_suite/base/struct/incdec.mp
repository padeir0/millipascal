struct List begin
    A:i8; B:i16; C:i32; D:i64;
end

const numItems = 16
data M:List [numItems]

proc main
var p:List, M_END:List
begin
    set M_END = M+sizeof[M];
    set p = M;
    do begin
        set p->A = 1ss;
        set p->B = 2s;
        set p->C = 3;
        set p->D = 4l;
        set p++;
    end while p < M_END;

    set p = M;
    do begin
        if p->A != 1ss begin
            exit 1ss;
        end
        if p->B != 2s begin
            exit 2ss;
        end
        if p->C != 3 begin
            exit 3ss;
        end
        if p->D != 4l begin
            exit 4ss;
        end
        set p++;
    end while p < M_END;
end

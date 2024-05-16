from ioutil import put_int, put_char, put_ln, put_i32

export setup, count, print_counts

struct I32A begin
    Num:i32;
end

const total_counters = 127ss;
data counters:I32A [total_counters];

proc setup[]
var i:i8
begin
    set i = 0ss;
    while i < total_counters begin
        set counters[i]->Num = 0;
        set i++;
    end
end

proc count[c:i8]
begin
    set counters[c]->Num++;
end

proc print_counts[]
var c:i8, num:i32
begin
    set c = 0ss;
    set num = 0;
    while c < total_counters begin
        set num = counters[c]->Num;
        if num > 0 begin
            put_char[c];
            put_char[':'];
            put_i32[num];
            put_ln[];
        end
        set c++;
    end
end

from ioutil import put_int, put_char

struct I32A begin
    num:i32;
end

data my_ints:I32A {
    5, ~10, 2, 50, ~9, 65, 0
}
const my_ints_length = sizeof[my_ints]/sizeof[I32A];

proc main
begin
    print_ints[my_ints, my_ints_length];
    insertion_sort[my_ints, my_ints_length];
    print_ints[my_ints, my_ints_length];
end

proc insertion_sort[array:I32A, length:i32]
var i, j:i32
begin
    set i = 1;
    while i < length begin
        set j = i;
        while j > 0 and
              array[j-1]->num > array[j]->num begin
            set array[j]->num <> array[j-1]->num;
            set j -= 1;
        end
        set i += 1;
    end
end

proc print_ints[array:I32A, length:i32]
var i:i32
begin
    set i = 0;
    while i < length begin
        put_int[array[i]->num:i64];
        put_char[' '];
        set i += 1;
    end
    put_char['\n'];
end

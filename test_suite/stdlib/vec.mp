struct Vec begin
    len, cap:i32
    buff:ptr
end

# returns NULL if i is out of range
proc index[v:Vec, i:i32] ptr
begin
end

# indexp panics if i is out of range
proc indexp[v:Vec, i:i32] ptr
var p:ptr
begin
    set p = index[v, i];
    if p == NULL begin
        fatal[OUTOFBOUNDS, sizeof[OUTOFBOUNDS]];
    end
    return p;
end

# returns if decrementing the length
# of the vector was sucessful, ie,
# if 0 <= len-1
proc dec[v:Vec] bool
begin
end

# returns if incrementing the length
# of the vector was sucessful, ie,
# if len+1 <= cap
proc inc[v:Vec] bool
begin
end

# concats a and b, stores in out
# returns false if the operation was unsucessful,
# ie, if out->cap < a->len + b->len
proc concat[a, b, out:Vec] bool
begin
    if out->cap < a->len + b->len begin
        return false;
    end
end

# slices v, stores the result into out,
# v and out share the underlying array.
proc slice[v:Vec, start, len:i32, out:Vec]
begin
end

# copies in into out,
# returns false if out->cap < in->len
proc copy[in, out:Vec] bool
begin
    if out->cap < in->len begin
        return false;
    end
end

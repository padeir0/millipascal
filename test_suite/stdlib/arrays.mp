# defines arrays and utils for built-in types
export all

struct I8A begin
	Num:i8;
end

struct I16A begin
	Num:i16;
end

struct I32A begin
	Num:i32;
end

struct I64A begin
	Num:i64;
end

struct U8A begin
	Num:u8;
end

struct U16A begin
	Num:u16;
end

struct U32A begin
	Num:u32;
end

struct U64A begin
	Num:u64;
end

struct PtrA begin
	Ptr:ptr;
end

struct BoolA begin
	Bool:bool;
end

proc copy_I32A[dest, source:I32A, size:i32]
var i:i32
begin
    set i = 0;
    while i < size begin
		set dest->Num = source->Num;
		set dest++;
		set source++;
        set i += 1;
    end
end

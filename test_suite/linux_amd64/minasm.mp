proc main
begin
    if MIN[] != ~4 begin
        exit 1ss;
    end
end

const INT_MIN = ~(1<<63)

data array {
    1, 5, ~4, 0, 7
}

proc MIN[] i64
var min:i64
asm [r9, r10, r11, r12, r13] # clobber set
begin
.start:
    _load r9, array;
    _load r10, {sizeof[array]};
    mov r11, 0;
    _store min, INT_MIN;
.loop_check:
    cmp r11, r10;
    jge min_ret;
.loop_body:
    mov r12, [r9, r11]@qword;
    _load r13, min;
    cmp r12, r13;
    jl loop_set_min; # execution continues as normal asm, falling into the next block
.loop_end:
    add r11, {sizeof[i64]};
    jmp loop_check;
.loop_set_min:
    _store min, r12;
    jmp loop_end;
.min_ret:
    _load r13, min;
    _ret r13;
end

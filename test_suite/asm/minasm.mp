proc main
begin
    if MIN[] != ~4l begin
        exit 3ss;
    end
end

const VERY_BIG = (1<<16):i64

const len = 5;

data array {
    1l, 5l, ~4l, 0l, 7l
}

proc MIN<stack>[] i64
asm begin
    push rbp;
    mov rbp, rsp;

    mov r12, {VERY_BIG};
    mov r11, array;
    mov r9, 0;
.loop_begin:
    mov r13, [r11]@qword;
    cmp r13, r12;
    jl loop_min;
    jmp loop_inc;
.loop_min:
    mov r12, r13;
.loop_inc:
    add r9, 1;
    add r11, {sizeof[i64]};
    cmp r9, {len};
    jl loop_begin;
.loop_end:
    mov [rbp, _ret0]@qword, r12;
    mov rsp, rbp;
    pop rbp;
    ret;
end

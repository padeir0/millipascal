proc main
begin
    if MIN[] != ~4l begin
        exit 1ss;
    end
end

const VERY_SMALL = (~(1<<16)):i64

data array {
    1, 5, ~4, 0, 7
}

proc MIN<stack>[] i64
var min:i64
asm
begin
.start:
    push rbp;
    mov rbp, rsp;

    mov r9, [rbp, array]@qword;
    mov r10, {sizeof[array]};
    mov r11, 0;
    mov [rbp, min]@qword, {VERY_SMALL};
.loop_check:
    cmp r11, r10;
    jge min_ret;
.loop_body:
    mov r12, [r9, r11]@qword;
    mov r13, [rbp, min]@qword;
    cmp r12, r13;
    jl loop_set_min; # execution continues as normal asm, falling into the next block
.loop_end:
    add r11, {sizeof[i64]};
    jmp loop_check;
.loop_set_min:
    mov [rbp, min]@qword, r12;
    jmp loop_end;
.min_ret:
    mov r13, [rbp, min]@qword;

    mov [rbp, _ret0]@qword, r13;
    mov rsp, rbp;
    pop rbp;
    ret;
end

data hello "Hello, World!\n"

proc main
begin
    print[hello, sizeof[hello]];
end

const begin
    SYS_WRITE = 1;
    STDOUT = 1;
end

proc print<stack>[p:ptr, size:i32] i32
asm begin
    push rbp;
    mov rbp, rsp;

    mov r0, {SYS_WRITE};
    mov r2d, [rbp, size]@dword;
    mov r6, [rbp, p]@qword;
    mov r7, {STDOUT};
    syscall;

    mov [rbp, _ret0]@qword, r0;
    mov rsp, rbp;
    pop rbp;
    ret;
end

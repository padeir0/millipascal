export write

data hello "Hello, World!\n"

proc main
begin
    write[hello, sizeof[hello]];
end

const begin
    SYS_WRITE = 1;
    STDOUT = 1;
end

attr abi_stack
proc write[p:ptr, size:i32] i32
asm [rdx, rsi, rdi, rax] # clobber set
begin
    push rbp;
    mov rbp, rsp;

    mov rdx, [rbp, size]@dword;
    mov rsi, [rbp, p]@qword;
    mov rdi, {STDOUT};
    mov rax, {SYS_WRITE};
    syscall;

    mov [rbp, _out0], rax;
    mov rsp, rbp;
    pop rbp;
    ret;
end

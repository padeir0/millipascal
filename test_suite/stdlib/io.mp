export write

const SYS_WRITE = 1
const STDOUT = 1

attr linux, amd64 # build tags for architecture
proc write[p:ptr, size:i64] i64
asm [rdx, rsi, rdi, rax] # clobber set
begin
    _load rdx, size; # pseudo-instructions start with '_'
    _load rsi, p;
    mov rdi, STDOUT;
    mov rax, SYS_WRITE;
    syscall;

    _ret rax;
end

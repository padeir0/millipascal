export print

const begin
    SYS_WRITE = 1;
    SYS_OPEN = 2;
    STDOUT = 1;
end

proc print<stack>[p:ptr, size:i32] i32
asm
begin
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

# type FD = u64

proc open[filename:ptr, flags:i32, mode:i32] u64, bool
var fd:u64
begin
    set fd = open_file[filename, flags, mode];

    if fd > ~4096ul begin # this is how linux maps errno values... weird right?
        return ~fd, false;
    end
    return fd, true;
end

proc open_file<stack>[filename:ptr, flags:i32, mode:i32] u64
asm
begin
    push rbp;
    mov rbp, rsp;

    mov r0, {SYS_OPEN};
    mov r2d, [rbp, mode]@dword;
    mov r6d, [rbp, flags]@dword;
    mov r7, [rbp, filename]@qword;
    syscall;

    mov [rbp, _ret0]@qword, r0;
    mov rsp, rbp;
    pop rbp;
    ret;
end

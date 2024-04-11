proc main
var i:i64
begin
    while i < 100l begin
        if loop[i] != i begin
            exit 1ss;
        end
        set i++;
    end
end

proc loop<stack> [i:i64] i64
asm begin
    push rbp;
    mov rbp, rsp;
    
    mov r9, [rbp, i]@qword;
    mov r10, 0;
.loop_begin:
    cmp r9, 0;
    jle loop_end;
.loop_body:
    sub r9, 1;
    add r10, 1;
    jmp loop_begin;
.loop_end:
    mov [rbp, _ret0]@qword, r10;

    mov rsp, rbp;
    pop rbp;
    ret;
end

struct Test begin
    A,B:u64;
    Res:u64;
end

data TESTS:Test {
    0ul, 0ul, 0ul,
    0ul, 1ul, 1ul,
    1ul, 2ul, 3ul,
    2ul, 4ul, 6ul,
    8ul, 4ul, 12ul,
    2ul, 3ul, 1ul,
    4ul, 6ul, 2ul,
}

proc main
var test, End:Test
begin
    set End = TESTS+sizeof[TESTS];
    set test = TESTS;
    do begin
        if XOR[test->A, test->B] != test->Res begin
            exit 1ss;
        end
        set test++;
    end while test < End;
end

proc XOR<stack> [a, b:u64] u64
asm begin
    push rbp;
    mov rbp, rsp;

    mov r10, [rbp, a]@qword;
    mov r11, [rbp, b]@qword;
    
    xor r10, r11;

    mov [rbp, _ret0]@qword, r10;
    mov rsp, rbp;
    pop rbp;
    ret;
end

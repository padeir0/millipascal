format ELF64 executable 3

; syscalls
SYS_WRITE = 1
SYS_EXIT  = 60

; files
STDOUT = 1

; automaton config
line_size = 102		; size of generations in cells (2 cells of padding)
max_gen   = 10000000	; computes at most this number of generations
alive     = 'O'		; char used to represent alive cells
dead      = ' '		; char used to represent dead cells

; rules, change this to encode different automatons
out_000 = dead
out_001 = alive
out_010 = alive
out_011 = alive
out_100 = dead
out_101 = alive
out_110 = alive
out_111 = dead

segment readable

newline db 0xA

segment readable writeable

line_A rb line_size	; double buffer
line_B rb line_size

segment readable executable

; r15 is a counter
; r14 holds the address to the current line/generation
; r13 holds the address to the next line/generation
entry $
	call		reset_buffers
	mov byte 	[line_A+line_size-1], alive

	mov	r15, 0		; var counter <- 0;
	mov	r14, line_A+1
	mov	r13, line_B+1
	
loop_start: 			; for i < max_gen => {

	push	r15
	push	r14
	push	r13
	call	compute_gen
	pop	r13
	pop	r14
	pop	r15

	mov	r12, r13	; swap current gen with next gen
	mov	r13, r14	; by swapping r13 with r14
	mov	r14, r12
	
	inc 	r15			; counter <- counter -1;
	cmp 	r15, max_gen
	jl	loop_start	; }
	
	mov	rdx, line_size-2	; print(current_gen);
	mov 	rsi, r14
	mov	rdi, STDOUT
	mov	rax, SYS_WRITE
	syscall
	
	mov	rdx, 1			; print("\n");
	mov	rsi, newline
	mov	rdi, STDOUT
	mov	rax, SYS_WRITE
	syscall
_end:
	xor	rdi, rdi 	; exit(0);
	mov	rax, SYS_EXIT
	syscall

; takes no arguments
; returns nothing
; registers:
;	r15 -> counter
;	r14 -> pointer into line_A
;	r13 -> pointer into line_B
reset_buffers:
	push 	rbp
	mov	rbp, rsp
	
	mov	r15, 1
	mov	r13, line_A
	mov	r14, line_B

reset_loop:
	mov byte	[r13], dead
	mov byte	[r14], dead 
	
	inc	r13		; increment pointers
	inc	r14
	
	inc	r15		; loop shenanigans
	cmp	r15, line_size-1
	jl	reset_loop
reset_ret:
	mov 	rsp, rbp
	pop 	rbp
	ret

; takes two arguments:
; 	start address of current generation (after padding)
; 	start address of next generation (after padding)
; returns nothing
; 
; registers:
; 	r15 -> pointer into next_gen line
; 	r14 -> pointer into curr_gen line
;	r13 -> counter
compute_gen:
	push 	rbp
	mov	rbp, rsp
	mov 	r15, [rbp+16]	; next_gen
	mov 	r14, [rbp+24]	; curr_gen
	
	mov	r13, 1

comp_loop:
	cmp byte	[r14 - 1], dead
	je		inp_0
inp_1:
	cmp byte	[r14], dead
	je		inp_10
inp_11:
	cmp byte	[r14+1], dead
	je		inp_110
inp_111:
	mov byte	[r15], out_111
	jmp comp_continue
inp_110:
	mov byte	[r15], out_110
	jmp comp_continue
inp_10:
	cmp byte	[r14+1], dead
	je		inp_100
inp_101:
	mov byte	[r15], out_101
	jmp comp_continue
inp_100:
	mov byte	[r15], out_100
	jmp comp_continue
	
inp_0:
	cmp byte	[r14], dead
	je		inp_00
inp_01:
	cmp byte	[r14 + 1], dead
	je		inp_010
inp_011:
	mov byte	[r15], out_011
	jmp comp_continue
inp_010:
	mov byte	[r15], out_010
	jmp comp_continue
inp_00:
	cmp byte	[r14 + 1], dead
	je		inp_000
inp_001:
	mov byte	[r15], out_001
	jmp comp_continue
inp_000:
	mov byte	[r15], out_000
	jmp comp_continue

comp_continue:
	inc	r15	; incrementing pointers
	inc	r14

	inc	r13	; loop shenanigans
	cmp	r13, line_size-2
	jl	comp_loop
	
comp_ret:
	mov 	rsp, rbp
	pop 	rbp
	ret


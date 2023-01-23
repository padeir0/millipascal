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
	mov byte 	[line_A+(line_size/2)], alive

	mov	r15, 0		; var counter <- 0;
	mov	r14, line_A+1
	mov	r13, line_B+1
	
loop_start: 			; for i < max_gen
	mov r12, r13
	mov r11, r14
	call	compute_gen

	xchg	r13, r14	; swap current gen with next gen
	
	inc 	r15			; counter <- counter -1;
	cmp 	r15, max_gen
	jl	loop_start	; }
	
	mov	rdx, line_size-2	; print(current_gen);
	mov	rsi, r14
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
;	r12 -> counter
;	r11 -> pointer into line_A
;	r10 -> pointer into line_B
reset_buffers:
	mov	r12, 0
	mov	r10, line_A
	mov	r11, line_B

reset_loop:
	mov byte	[r10], dead
	mov byte	[r11], dead 
	
	inc	r10		; increment pointers
	inc	r11
	
	inc	r12		; loop shenanigans
	cmp	r12, line_size
	jl	reset_loop
reset_ret:
	ret

; takes two arguments:
; 	start address of current generation (after padding)
; 	start address of next generation (after padding)
; returns nothing
; 
; registers:
; 	r12 -> pointer into next_gen line
; 	r11 -> pointer into curr_gen line
;	r10 -> counter
compute_gen:
	mov	r10, 0

comp_loop:
	cmp byte	[r11 - 1], dead
	je		inp_0
inp_1:
	cmp byte	[r11], dead
	je		inp_10
inp_11:
	cmp byte	[r11+1], dead
	je		inp_110
inp_111:
	mov byte	[r12], out_111
	jmp comp_continue
inp_110:
	mov byte	[r12], out_110
	jmp comp_continue
inp_10:
	cmp byte	[r11+1], dead
	je		inp_100
inp_101:
	mov byte	[r12], out_101
	jmp comp_continue
inp_100:
	mov byte	[r12], out_100
	jmp comp_continue
	
inp_0:
	cmp byte	[r11], dead
	je		inp_00
inp_01:
	cmp byte	[r11 + 1], dead
	je		inp_010
inp_011:
	mov byte	[r12], out_011
	jmp comp_continue
inp_010:
	mov byte	[r12], out_010
	jmp comp_continue
inp_00:
	cmp byte	[r11 + 1], dead
	je		inp_000
inp_001:
	mov byte	[r12], out_001
	jmp comp_continue
inp_000:
	mov byte	[r12], out_000
	jmp comp_continue

comp_continue:
	inc	r12	; incrementing pointers
	inc	r11

	inc	r10	; loop shenanigans
	cmp	r10, line_size-2
	jl	comp_loop
	
comp_ret:
	ret

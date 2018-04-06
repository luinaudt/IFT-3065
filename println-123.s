 .text
 .globl _main
 .globl main
_main:
main:
push $100*1024*1024
call mmap
mov %rax, %r11
#proclam0
lea lam0(%rip), %rax
push %rax
#make closure
push $8  # nfree + 1
pop  8*0(%r11)
push $0  # type
pop  8*1(%r11)
pop  8*2(%r11)
push %r11
add  $8*2+1, (%rsp)
add  $8*3, %r11
#println
pop glob_0(%rip)
#proclam1
lea lam1(%rip), %rax
push %rax
#make closure
push $8  # nfree + 1
pop  8*0(%r11)
push $0  # type
pop  8*1(%r11)
pop  8*2(%r11)
push %r11
add  $8*2+1, (%rsp)
add  $8*3, %r11
#+
pop glob_1(%rip)
push $8*123
call print_ln 
push $10
call putchar
push $0
push glob_0(%rip)
#proclam2

push $125*8
push glob_0(%rip)
#push $125*8
mov  (%rsp), %rdi
lea return0(%rip), %rax
push %rax
mov $2,%rax
jmp *-1(%rdi)	

	
lea lam2(%rip), %rax
push %rax
mov (%rsp), %rdi
lea return0(%rip), %rax
push %rax
mov $1, %rax
jmp *-1(%rdi)

.align 8
.quad 0
.quad 12
.byte 0
return0:
add $8*2,%rsp 
 mov $0, %rax
 ret
 


.align 8
.quad 0
.quad 0
.byte 0
lam2:
cmp $1, %rax
jnz nargs_error
push 8*1(%rsp)
push $8*125
#closure-code
push 8*4(%rsp)
mov (%rsp), %rdi
lea return1(%rip), %rax
push %rax
mov $2, %rax
jmp *-1(%rdi)

.align 8
.quad 0
.quad 12
.byte 0
return1:
mov 8*3(%rsp),%rdi
mov (%rsp), %rax
add $8*3,%rsp
push %rax
jmp *%rdi

.align 8
.quad 0
.quad 0
.byte 0
lam1:
cmp $3, %rax
jnz nargs_error
push 8*3(%rsp)
push 8*3(%rsp)
pop %rax 
add %rax, (%rsp)
mov 8*1(%rsp),%rdi
mov (%rsp), %rax
add $8*5,%rsp
push %rax
jmp *%rdi

.align 8
.quad 0
.quad 0
.byte 0
lam0:
cmp $2, %rax
jnz nargs_error
push 8*2(%rsp)
call print_ln 
push $10
call putchar
push $0
mov 8*1(%rsp),%rdi
mov (%rsp), %rax
add $8*4,%rsp
push %rax
jmp *%rdi
add $8*2,%rsp 

nargs_error:
mov $1, %rax
ret


.data
 .align 8
glob_1: .quad 0
glob_0: .quad 0

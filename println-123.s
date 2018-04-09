.text
.globl _main
.globl main
_main:
main:
push %rbp 
mov %rsp, %rbp
  push  $100*1024*1024
  call  mmap
  mov   %rax, %r10

# fs = 0 (proc lam_0)
  lea   lam_0(%rip), %rax
  push  %rax

# fs = 1 (make closure)
  pop   8*2(%r10)  # ptr-code
  push  $0
  pop   8*1(%r10)  # type
  push  $8*1
  pop   8*0(%r10)  # longueur
  push  %r10
  add   $8*2+1, (%rsp)
  add   $8*3, %r10

# fs = 1 (def println)
  pop   glob_0(%rip)

# fs = 0 (proc lam_1)
  lea   lam_1(%rip), %rax
  push  %rax

# fs = 1 (make closure)
  pop   8*2(%r10)  # ptr-code
  push  $0
  pop   8*1(%r10)  # type
  push  $8*1
  pop   8*0(%r10)  # longueur
  push  %r10
  add   $8*2+1, (%rsp)
  add   $8*3, %r10

# fs = 1 (def +)
  pop   glob_1(%rip)

# fs = 0 (re-def println)

# fs = 0 (proc lam_2)
  lea   lam_2(%rip), %rax
  push  %rax

# fs = 1 (make closure)
  pop   8*2(%r10)  # ptr-code
  push  $0
  pop   8*1(%r10)  # type
  push  $8*1
  pop   8*0(%r10)  # longueur
  push  %r10
  add   $8*2+1, (%rsp)
  add   $8*3, %r10
  pop   glob_0(%rip)

# fs = 0 (let)
  push  glob_0(%rip)

# fs = 1 (closure-code$clo$clo123)

# fs = 1 (the arguments)
  push  8*0(%rsp)

# fs = 2 (2 pushloc)
  push  $8*123

# fs = 3 (proc-code ptr)
  push  8*2(%rsp)

# fs = 4 (the call)
  mov   (%rsp), %rdi
  lea   return_0(%rip), %rax
  push  %rax
  mov   $2, %rax
  jmp   *-1(%rdi)
.align 8
.quad 0
.quad 12
.byte 0
return_0:
  pop   %rax        # pop result
  add   $8*1, %rsp  # adjust stack
  push  %rax        # push result
  pop   %rax        # pop result
  add   $8*1, %rsp  # adjust stack
  mov   $0, %rax
mov %rbp, %rsp
pop %rbp
  ret



# fs = 0 (lambda lam_23)
.align 8
.quad 0
.quad 0
.byte 0
lam_2:
  cmp   $2, %rax
  jnz   nargs_error
  push  8*2(%rsp)

# fs = 4 (4 pushloc)
  call  print_ln
  push  $10
  call  putchar
  push  $0
  mov   8*1(%rsp), %rdi
  mov   (%rsp), %rax
  add   $8*4, %rsp
  push  %rax
  jmp   *%rdi

# fs = 0 (lambda lam_14)
.align 8
.quad 0
.quad 0
.byte 0
lam_1:
  cmp   $3, %rax
  jnz   nargs_error
  push  8*3(%rsp)

# fs = 5 (5 pushloc)
  push  8*3(%rsp)

# fs = 6 (6 pushloc)
  pop   %rax
  add   %rax, (%rsp)
  mov   8*1(%rsp), %rdi
  mov   (%rsp), %rax
  add   $8*5, %rsp
  push  %rax
  jmp   *%rdi

# fs = 0 (lambda lam_03)
.align 8
.quad 0
.quad 0
.byte 0
lam_0:
  cmp   $2, %rax
  jnz   nargs_error
  push  8*2(%rsp)

# fs = 4 (4 pushloc)
  call  print_ln
  push  $10
  call  putchar
  push  $0
  mov   8*1(%rsp), %rdi
  mov   (%rsp), %rax
  add   $8*4, %rsp
  push  %rax
  jmp   *%rdi

nargs_error:
  mov   $1, %rax
  ret


.data
.align 8
glob_1: .quad 0
glob_0: .quad 0

 .text
 .globl _main
 .globl main
_main:
main:
 push $6
 push $6
 pop %rbx
 pop %rax
 add  %rbx, %rax
 push %rax
 push $13
 push $25
 pop %rbx
 pop %rax
 add  %rbx, %rax
 push %rax
 pop %rbx
 pop %rax
 add  %rbx, %rax
 push %rax
 call print_word_dec
 push $10
 call putchar
 mov $0, %rax
 ret

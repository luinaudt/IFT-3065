 .text
 .globl _main
 .globl main
_main:
main:
 push $123
 push $12
 pop %rbx
 pop %rax
 add  %rbx, %rax
 push %rax
 call print_word_dec
 push $10
 call putchar
 mov $0, %rax
 ret

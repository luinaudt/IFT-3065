 .text
 .globl _main
 .globl main
_main:
main:
 push $123
 call print_word_dec
 push $10
 call putchar
 mov $0, %rax
 ret

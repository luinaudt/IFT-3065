 .text
 .globl _main
 .globl main
_main:
main:
 push $101
 call print_word_dec
 push $10
 call putchar
 push $0
 call print_word_dec
 push $10
 call putchar
 push $-314159
 call print_word_dec
 push $10
 call putchar
 push $-78
 call print_word_dec
 push $10
 call putchar
 mov $0, %rax
 ret

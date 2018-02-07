 .text
 .globl _main
 .globl main
_main:
main:
 push $1
 push $2
 pop %rbx
 pop %rax
 add  %rbx, %rax
 push %rax
 call print_word_dec
 push $10
 call putchar
 push $1
 push $2
 pop %rbx
 pop %rax
 sub  %rbx, %rax
 push %rax
 call print_word_dec
 push $10
 call putchar
 push $1
 push $2
 pop %rbx
 pop %rax
 mul  %rbx
 push %rax
 call print_word_dec
 push $10
 call putchar
 push $1
 push $2
 pop %rbx
 pop %rax
 cqo
 idiv %rbx
 push %rax
 call print_word_dec
 push $10
 call putchar
 push $1
 push $2
 pop %rbx
 pop %rax
 cqo 
 idiv %rbx
 push %rdx
 call print_word_dec
 push $10
 call putchar
 push $2
 push $-5
 push $6
 pop %rbx
 pop %rax
 sub  %rbx, %rax
 push %rax
 pop %rbx
 pop %rax
 add  %rbx, %rax
 push %rax
 push $2
 pop %rbx
 pop %rax
 mul  %rbx
 push %rax
 call print_word_dec
 push $10
 call putchar
 mov $0, %rax
 ret

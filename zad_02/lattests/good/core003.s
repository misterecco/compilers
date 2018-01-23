.section .data
.section .text
    .p2align 4,,15
    .globl main
    .type main, @function
f:
    pushq %RBP
    movq %RSP, %RBP
    movq $0, %RAX
    leave
    ret
g:
    pushq %RBP
    movq %RSP, %RBP
    movq $0, %RAX
    leave
    ret
p:
    pushq %RBP
    movq %RSP, %RBP
    leave
    ret
main:
    pushq %RBP
    movq %RSP, %RBP
    call p
    movq $0, %RAX
    leave
    ret

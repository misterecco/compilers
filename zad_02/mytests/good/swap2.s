.section .data
.section .text
    .p2align 4,,15
    .globl main
    .type main, @function
main:
    pushq %RBP
    movq %RSP, %RBP
    movq $1, %RDI
    movq $50, %RSI
    movq $100, %RDX
    call f
    movq $0, %RAX
    leave
    ret
f:
    pushq %RBP
    movq %RSP, %RBP
    addq $-8, %RSP
    pushq %RBX
    pushq %R12
    pushq %R13
    cmpq $1, %RDI
    je .t_lbl_0
    movq %RSI, %RBX
    movq %RDX, %R12
    jmp .lbl_1
.t_lbl_0:
    movq %RSI, %RBX
    movq %RDX, %R12
    jmp .lbl_0
.lbl_0:
    xchgq %RBX, %R12
    jmp .lbl_1
.lbl_1:
    movq %RBX, %R13
    subq %R12, %R13
    movq %R13, %RDI
    call printInt
    popq %R13
    popq %R12
    popq %RBX
    leave
    ret

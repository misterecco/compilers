.section .data
.section .text
    .p2align 4,,15
    .globl main
    .type main, @function
main:
    pushq %RBP
    movq %RSP, %RBP
    addq $-8, %RSP
    pushq %RBX
    pushq %R12
    pushq %R13
    movq $1, %RBX
    movq $1, %R12
    movq $1, %R13
    jmp .lbl_1
.lbl_0:
    movq %RBX, %R13
    imulq %R12, %R13
    movq %R12, %RBX
    addq $1, %RBX
    movq %RBX, %R12
    movq %R13, %RBX
    jmp .lbl_1
.lbl_1:
    cmpq $10, %R12
    jl .t_lbl_0
    movq %R13, %RBX
    jmp .lbl_2
.t_lbl_0:
    jmp .lbl_0
.lbl_2:
    movq %RBX, %RDI
    call printInt
    movq $0, %RAX
    popq %R13
    popq %R12
    popq %RBX
    leave
    ret

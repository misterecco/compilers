.section .data
.section .text
    .p2align 4,,15
    .globl main
    .type main, @function
main:
    pushq %RBP
    movq %RSP, %RBP
    pushq %RBX
    pushq %R12
    pushq %R13
    pushq %R14
    movq $1, %RDI
    call printInt
    movq $1, %RBX
    movq $1, %R12
    movq $5000000, %R13
    jmp .lbl_1
.lbl_0:
    movq %R12, %RDI
    call printInt
    movq %RBX, %R14
    addq %R12, %R14
    movq %R14, %R12
    subq %RBX, %R12
    movq %R12, %RBX
    movq %R14, %R12
    jmp .lbl_1
.lbl_1:
    cmpq %R13, %R12
    jl .t_lbl_0
    jmp .lbl_2
.t_lbl_0:
    jmp .lbl_0
.lbl_2:
    movq $0, %RAX
    popq %R14
    popq %R13
    popq %R12
    popq %RBX
    leave
    ret

.section .data
.str_0:
    .string "--------------"
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
    pushq %R14
    pushq %R15
    movq $1, %RBX
    movq $1, %R12
    jmp .lbl_1
.lbl_0:
    movq %RBX, %R13
    imulq %R12, %R13
    leaq .str_0, %RDI
    call printString
    movq %R12, %RDI
    call printInt
    movq %R13, %RDI
    call printInt
    movq %R12, %RBX
    addq $1, %RBX
    movq %RBX, %R12
    movq %R13, %RBX
    jmp .lbl_1
.lbl_1:
    cmpq $10, %R12
    jl .t_lbl_0
    jmp .lbl_2
.t_lbl_0:
    jmp .lbl_0
.lbl_2:
    movq $0, %RAX
    popq %R15
    popq %R14
    popq %R13
    popq %R12
    popq %RBX
    leave
    ret

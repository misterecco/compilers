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
    movq $4, %RAX
    cmpq $6, %RAX
    jl .t_lbl_0
    jmp .lbl_1
.t_lbl_0:
    jmp .lbl_0
.lbl_0:
    movq $20, %RBX
    movq $10, %R12
    jmp .lbl_2
.lbl_1:
    movq $1, %RBX
    movq $2, %R12
    jmp .lbl_2
.lbl_2:
    movq %RBX, %RDI
    call printInt
    movq %R12, %RDI
    call printInt
    movq $0, %RAX
    popq %R12
    popq %RBX
    leave
    ret

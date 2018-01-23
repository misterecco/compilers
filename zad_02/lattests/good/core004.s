.section .data
.section .text
    .p2align 4,,15
    .globl main
    .type main, @function
main:
    pushq %RBP
    movq %RSP, %RBP
    movq $1, %RAX
    cmpq $1, %RAX
    je .t_lbl_0
    jmp .lbl_1
.t_lbl_0:
    jmp .lbl_0
.lbl_0:
    movq $42, %RDI
    call printInt
    jmp .lbl_1
.lbl_1:
    movq $0, %RAX
    leave
    ret

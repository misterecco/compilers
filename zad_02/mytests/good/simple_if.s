.section .data
.section .text
    .p2align 4,,15
    .globl main
    .type main, @function
main:
    pushq %RBP
    movq %RSP, %RBP
    movq $0, %RAX
    cmpq $1, %RAX
    jg .t_lbl_0
    jmp .lbl_1
.t_lbl_0:
    jmp .lbl_0
.lbl_0:
    movq $10, %RDI
    call printInt
    jmp .lbl_2
.lbl_1:
    movq $20, %RDI
    call printInt
    jmp .lbl_2
.lbl_2:
    movq $0, %RAX
    leave
    ret

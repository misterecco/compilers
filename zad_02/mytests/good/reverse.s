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
    call readInt
    movq %RAX, %RBX
    cmpq $42, %RBX
    jne .t_lbl_0
    jmp .lbl_1
.t_lbl_0:
    jmp .lbl_2
.lbl_2:
    call main
    movq %RAX, %R12
    cmpq $0, %R12
    je .t_lbl_1
    jmp .lbl_1
.t_lbl_1:
    jmp .lbl_0
.lbl_0:
    movq %RBX, %RDI
    call printInt
    jmp .lbl_1
.lbl_1:
    movq $0, %RAX
    popq %R12
    popq %RBX
    leave
    ret

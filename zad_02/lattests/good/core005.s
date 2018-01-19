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
    pushq %R14
    pushq %R15
    movq $56, %RBX
    addq $45, %RBX
    cmpq $2, %RBX
    jle .t_lbl_0
    jmp .lbl_1
.t_lbl_0:
    jmp .lbl_0
.lbl_0:
    movq $1, %RBX
    jmp .lbl_2
.lbl_1:
    movq $2, %RBX
    jmp .lbl_2
.lbl_2:
    movq %RBX, %RDI
    call printInt
    movq $0, %RAX
    popq %R15
    popq %R14
    popq %R13
    popq %R12
    popq %RBX
    leave
    ret

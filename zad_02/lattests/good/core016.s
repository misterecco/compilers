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
    movq $17, %RBX
    jmp .lbl_1
.lbl_0:
    movq %RBX, %R12
    subq $2, %R12
    movq %R12, %RBX
    jmp .lbl_1
.lbl_1:
    cmpq $0, %RBX
    jg .t_lbl_0
    jmp .lbl_2
.t_lbl_0:
    jmp .lbl_0
.lbl_2:
    cmpq $0, %RBX
    jl .t_lbl_1
    jmp .lbl_4
.t_lbl_1:
    jmp .lbl_3
.lbl_3:
    movq $0, %RDI
    call printInt
    movq $0, %RAX
    popq %R12
    popq %RBX
    leave
    ret
.lbl_4:
    movq $1, %RDI
    call printInt
    movq $0, %RAX
    popq %R12
    popq %RBX
    leave
    ret
.lbl_5:
    popq %R12
    popq %RBX
    leave
    ret

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
    movq $17, %RDI
    call ev
    movq %RAX, %RBX
    movq %RBX, %RDI
    call printInt
    movq $0, %RAX
    popq %RBX
    leave
    ret
ev:
    pushq %RBP
    movq %RSP, %RBP
    pushq %RBX
    pushq %R12
    cmpq $0, %RDI
    jg .t_lbl_0
    movq %RDI, %RBX
    jmp .lbl_1
.t_lbl_0:
    movq %RDI, %RBX
    jmp .lbl_0
.lbl_0:
    movq %RBX, %R12
    subq $2, %R12
    movq %R12, %RDI
    call ev
    movq %RAX, %RBX
    movq %RBX, %RAX
    popq %R12
    popq %RBX
    leave
    ret
.lbl_1:
    cmpq $0, %RBX
    jl .t_lbl_1
    jmp .lbl_4
.t_lbl_1:
    jmp .lbl_3
.lbl_3:
    movq $0, %RAX
    popq %R12
    popq %RBX
    leave
    ret
.lbl_4:
    movq $1, %RAX
    popq %R12
    popq %RBX
    leave
    ret
.lbl_5:
    jmp .lbl_2
.lbl_2:
    popq %R12
    popq %RBX
    leave
    ret

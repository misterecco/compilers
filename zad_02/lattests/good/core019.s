.section .data
.str_0:
    .string "foo"
.section .text
    .p2align 4,,15
    .globl main
    .type main, @function
main:
    pushq %RBP
    movq %RSP, %RBP
    pushq %RBX
    pushq %R12
    movq $1, %RDI
    call printInt
    movq $78, %RDI
    call printInt
    movq $78, %RBX
    jmp .lbl_1
.lbl_0:
    movq %RBX, %R12
    subq $1, %R12
    movq %R12, %RDI
    call printInt
    movq %R12, %RBX
    addq $7, %RBX
    movq %RBX, %RDI
    call printInt
    movq %R12, %RBX
    jmp .lbl_1
.lbl_1:
    cmpq $76, %RBX
    jg .t_lbl_0
    jmp .lbl_2
.t_lbl_0:
    jmp .lbl_0
.lbl_2:
    movq %RBX, %RDI
    call printInt
    cmpq $4, %RBX
    jg .t_lbl_1
    jmp .lbl_4
.t_lbl_1:
    jmp .lbl_3
.lbl_3:
    movq $4, %RDI
    call printInt
    jmp .lbl_5
.lbl_4:
    leaq .str_0, %RDI
    call printString
    jmp .lbl_5
.lbl_5:
    movq %RBX, %RDI
    call printInt
    movq $0, %RAX
    popq %R12
    popq %RBX
    leave
    ret

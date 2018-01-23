.section .data
.str_1:
    .string " "
.str_2:
    .string "concatenation"
.str_4:
    .string "false"
.str_0:
    .string "string"
.str_3:
    .string "true"
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
    movq $23, %RBX
    imulq $-1, %RBX
    movq $56, %R12
    addq %RBX, %R12
    movq %R12, %RDI
    call printInt
    movq $56, %R12
    subq %RBX, %R12
    movq %R12, %RDI
    call printInt
    movq $56, %R12
    imulq %RBX, %R12
    movq %R12, %RDI
    call printInt
    movq $45, %RAX
    cqo
    movq $2, %R10
    idivq %R10
    movq %RAX, %R12
    movq %R12, %RDI
    call printInt
    movq $78, %RAX
    cqo
    movq $3, %R10
    idivq %R10
    movq %RDX, %R12
    movq %R12, %RDI
    call printInt
    movq $56, %R12
    subq %RBX, %R12
    movq $56, %R13
    addq %RBX, %R13
    cmpq %R13, %R12
    jg .t_lbl_0
    jmp .lbl_1
.t_lbl_0:
    jmp .lbl_0
.lbl_0:
    movq $1, %R12
    movq $56, %R13
    jmp .lbl_2
.lbl_1:
    movq $0, %R12
    movq $56, %R13
    jmp .lbl_2
.lbl_2:
    movq %R12, %RDI
    call printBool
    movq %R13, %RAX
    cqo
    idivq %RBX
    movq %RAX, %R12
    movq %R13, %R14
    imulq %RBX, %R14
    cmpq %R14, %R12
    jle .t_lbl_1
    jmp .lbl_4
.t_lbl_1:
    jmp .lbl_3
.lbl_3:
    movq $1, %RBX
    jmp .lbl_5
.lbl_4:
    movq $0, %RBX
    jmp .lbl_5
.lbl_5:
    movq %RBX, %RDI
    call printBool
    leaq .str_0, %RDI
    leaq .str_1, %RSI
    call __concat__
    movq %RAX, %RBX
    movq %RBX, %RDI
    leaq .str_2, %RSI
    call __concat__
    movq %RAX, %R12
    movq %R12, %RDI
    call printString
    movq $0, %RAX
    popq %R14
    popq %R13
    popq %R12
    popq %RBX
    leave
    ret
printBool:
    pushq %RBP
    movq %RSP, %RBP
    addq $-8, %RSP
    pushq %RBX
    cmpq $1, %RDI
    je .t_lbl_2
    jmp .lbl_7
.t_lbl_2:
    jmp .lbl_6
.lbl_6:
    leaq .str_3, %RDI
    call printString
    popq %RBX
    leave
    ret
.lbl_7:
    leaq .str_4, %RDI
    call printString
    popq %RBX
    leave
    ret
.lbl_8:
    popq %RBX
    leave
    ret

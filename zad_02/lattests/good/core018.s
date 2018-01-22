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
    pushq %R13
    pushq %R14
    call readInt
    movq %RAX, %RBX
    call readString
    movq %RAX, %R12
    call readString
    movq %RAX, %R13
    movq %RBX, %R14
    subq $5, %R14
    movq %R14, %RDI
    call printInt
    movq %R12, %RDI
    movq %R13, %RSI
    call __concat__
    movq %RAX, %RBX
    movq %RBX, %RDI
    call printString
    movq $0, %RAX
    popq %R14
    popq %R13
    popq %R12
    popq %RBX
    leave
    ret

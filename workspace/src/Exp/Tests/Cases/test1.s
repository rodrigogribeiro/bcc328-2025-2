.text
.globl main
main:
li x5, 1
li x6, 3
li x7, 5
mul x8, x6, x7
add x9, x5, x8
li x10, 6
add x11, x9, x10
mv x10, x11
call print_int
li x10, 0
call exit_program
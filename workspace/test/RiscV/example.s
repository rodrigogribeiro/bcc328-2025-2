.data
buffer: .space 20
newline: .ascii "\n"

.text
.globl _start
_start:
    # Generated RISC-V code for expression evaluation
    li t0, 5
    li t1, 3
    add t2, t0, t1
    li t3, 2
    mul t4, t2, t3
    # Convert integer to string and print
    mv t0, t4
    la t1, buffer
    addi t2, t1, 19
    li t3, 0
    sb t3, t2, 0
    addi t2, t2, -1
convert_loop:
    li t4, 10
    # Integer division by 10 (simplified - using modulo logic)
    li a0, 0
    mv a1, t0
div_loop:
    blt a1, t4, div_done
    sub a1, a1, t4
    addi a0, a0, 1
    j div_loop
div_done:
    addi a1, a1, 48
    sb a1, t2, 0
    addi t2, t2, -1
    mv t0, a0
    bne t0, zero, convert_loop
    beq t0, zero, print_setup
    li t3, 48
    sb t3, t2, 0
    addi t2, t2, -1
print_setup:
    addi t2, t2, 1
    la a2, buffer
    addi a2, a2, 19
    sub a2, a2, t2
    # System call: sys_write(1, buffer+offset, length)
    li a7, 64
    li a0, 1
    mv a1, t2
    ecall
    # Print newline
    la a1, newline
    li a2, 1
    li a7, 64
    li a0, 1
    ecall
    # Exit program
    li a7, 93
    li a0, 0
    ecall

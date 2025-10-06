(module
  ;; Import WASI functions
  (import "wasi_snapshot_preview1" "fd_write" (func $fd_write (param i32 i32 i32 i32) (result i32)))
  (import "wasi_snapshot_preview1" "fd_read" (func $fd_read (param i32 i32 i32 i32) (result i32)))

  ;; Memory for I/O buffers and string operations
  (memory (export "memory") 1)

  ;; Memory layout:
  ;; 0-15: iovec structure for fd_write/fd_read
  ;; 16-31: return value storage
  ;; 32-63: input buffer (32 bytes)
  ;; 64-95: output buffer (32 bytes)

  ;; Function to convert integer to string and write to stdout
  ;; Param: i32 - the integer to print
  (func $print_int (export "print_int") (param $value i32)
    (local $temp i32)
    (local $digit i32)
    (local $len i32)
    (local $neg i32)
    (local $pos i32)

    ;; Handle negative numbers
    local.get $value
    i32.const 0
    i32.lt_s
    local.set $neg

    ;; Get absolute value
    local.get $value
    local.get $neg
    if (result i32)
      i32.const 0
      local.get $value
      i32.sub
    else
      local.get $value
    end
    local.set $temp

    ;; Convert to string (reverse order first)
    i32.const 95  ;; Start at end of output buffer
    local.set $pos

    ;; Handle zero case
    local.get $temp
    i32.eqz
    if
      local.get $pos
      i32.const 48  ;; ASCII '0'
      i32.store8
      local.get $pos
      i32.const 1
      i32.sub
      local.set $pos
      i32.const 1
      local.set $len
    else
      ;; Convert digits
      loop $digit_loop
        local.get $temp
        i32.eqz
        br_if $digit_loop

        ;; Get digit
        local.get $temp
        i32.const 10
        i32.rem_u
        i32.const 48  ;; ASCII '0'
        i32.add
        local.set $digit

        ;; Store digit
        local.get $pos
        local.get $digit
        i32.store8

        ;; Update position and temp
        local.get $pos
        i32.const 1
        i32.sub
        local.set $pos

        local.get $temp
        i32.const 10
        i32.div_u
        local.set $temp

        ;; Increment length
        local.get $len
        i32.const 1
        i32.add
        local.set $len

        br $digit_loop
      end
    end

    ;; Add negative sign if needed
    local.get $neg
    if
      local.get $pos
      i32.const 45  ;; ASCII '-'
      i32.store8
      local.get $pos
      i32.const 1
      i32.sub
      local.set $pos
      local.get $len
      i32.const 1
      i32.add
      local.set $len
    end

    ;; Set up iovec structure
    i32.const 0
    local.get $pos
    i32.const 1
    i32.add  ;; Start of actual string
    i32.store

    i32.const 4
    local.get $len
    i32.store

    ;; Call fd_write (stdout = 1)
    i32.const 1    ;; fd (stdout)
    i32.const 0    ;; iovs pointer
    i32.const 1    ;; iovs_len
    i32.const 16   ;; nwritten pointer
    call $fd_write
    drop
    drop
  )

  ;; Function to read integer from stdin
  ;; Returns: i32 - the integer read from console
  (func $read_int (export "read_int") (result i32)
    (local $bytes_read i32)
    (local $result i32)
    (local $pos i32)
    (local $char i32)
    (local $neg i32)

    ;; Clear input buffer
    i32.const 32
    i32.const 0
    i32.const 32
    memory.fill

    ;; Set up iovec for reading
    i32.const 0
    i32.const 32  ;; Buffer start
    i32.store

    i32.const 4
    i32.const 31  ;; Buffer size (leave space for null terminator)
    i32.store

    ;; Read from stdin
    i32.const 0    ;; fd (stdin)
    i32.const 0    ;; iovs pointer
    i32.const 1    ;; iovs_len
    i32.const 16   ;; nread pointer
    call $fd_read
    drop           ;; Drop the return value from fd_read

    ;; Get number of bytes read
    i32.const 16
    i32.load
    local.set $bytes_read

    ;; Parse the integer
    i32.const 32  ;; Start of buffer
    local.set $pos
    i32.const 0
    local.set $result
    i32.const 0
    local.set $neg

    ;; Check for negative sign
    local.get $pos
    i32.load8_u
    i32.const 45  ;; ASCII '-'
    i32.eq
    if
      i32.const 1
      local.set $neg
      local.get $pos
      i32.const 1
      i32.add
      local.set $pos
    end

    ;; Parse digits
    loop $parse_loop
      local.get $pos
      i32.load8_u
      local.set $char

      ;; Check if character is a digit
      local.get $char
      i32.const 48  ;; ASCII '0'
      i32.ge_u
      local.get $char
      i32.const 57  ;; ASCII '9'
      i32.le_u
      i32.and
      if
        ;; Multiply result by 10 and add digit
        local.get $result
        i32.const 10
        i32.mul
        local.get $char
        i32.const 48
        i32.sub
        i32.add
        local.set $result

        ;; Move to next character
        local.get $pos
        i32.const 1
        i32.add
        local.set $pos

        br $parse_loop
      end
    end

    ;; Apply negative sign if needed and return the result
    local.get $neg
    if (result i32)
      i32.const 0
      local.get $result
      i32.sub
    else
      local.get $result
    end
  )

  ;; Function to print integer with newline
  (func $print_int_ln (export "print_int_ln") (param $value i32)
    local.get $value
    call $print_int

    ;; Print newline
    i32.const 64
    i32.const 10  ;; ASCII newline
    i32.store8

    ;; Set up iovec for newline
    i32.const 0
    i32.const 64
    i32.store

    i32.const 4
    i32.const 1
    i32.store

    ;; Write newline
    i32.const 1    ;; fd (stdout)
    i32.const 0    ;; iovs pointer
    i32.const 1    ;; iovs_len
    i32.const 16   ;; nwritten pointer
    call $fd_write
    drop
  )
)

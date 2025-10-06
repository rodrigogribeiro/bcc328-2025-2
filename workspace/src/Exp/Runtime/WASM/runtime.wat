(module
  ;; Import WASI fd_write function
  (import "wasi_snapshot_preview1" "fd_write"
    (func $fd_write (param i32 i32 i32 i32) (result i32)))

  ;; Memory export for library usage
  (memory (export "memory") 1)

  ;; Buffer for storing the integer string (128 bytes starting at offset 0)
  (global $buffer_start i32 (i32.const 0))

  ;; IOVec structure (at offset 128)
  (global $iovec_start i32 (i32.const 128))

  ;; Result buffer for nwritten (at offset 144)
  (global $nwritten_ptr i32 (i32.const 144))

  ;; Global to store position result from convert_digits
  (global $pos_result (mut i32) (i32.const 0))

  ;; EXPORTED LIBRARY FUNCTION: print_int
  ;; Converts integer to string and prints it to stdout with newline
  (func (export "print_int") (param $value i32)
    (local $pos i32)
    (local $digit i32)
    (local $is_negative i32)
    (local $len i32)
    (local $start_pos i32)

    ;; Start building string from the end of buffer
    (local.set $pos (i32.add (global.get $buffer_start) (i32.const 63)))

    ;; Check if number is negative
    (local.set $is_negative (i32.lt_s (local.get $value) (i32.const 0)))

    ;; Make value positive for digit extraction
    (if (local.get $is_negative)
      (then
        ;; Handle INT32_MIN case specially to avoid overflow
        (if (i32.eq (local.get $value) (i32.const 0x80000000))
          (then
            ;; Store "8463847412-" backwards (2147483648 reversed)
            (i32.store8 (local.get $pos) (i32.const 56))  ;; '8'
            (local.set $pos (i32.sub (local.get $pos) (i32.const 1)))
            (i32.store8 (local.get $pos) (i32.const 52))  ;; '4'
            (local.set $pos (i32.sub (local.get $pos) (i32.const 1)))
            (i32.store8 (local.get $pos) (i32.const 54))  ;; '6'
            (local.set $pos (i32.sub (local.get $pos) (i32.const 1)))
            (i32.store8 (local.get $pos) (i32.const 51))  ;; '3'
            (local.set $pos (i32.sub (local.get $pos) (i32.const 1)))
            (i32.store8 (local.get $pos) (i32.const 56))  ;; '8'
            (local.set $pos (i32.sub (local.get $pos) (i32.const 1)))
            (i32.store8 (local.get $pos) (i32.const 52))  ;; '4'
            (local.set $pos (i32.sub (local.get $pos) (i32.const 1)))
            (i32.store8 (local.get $pos) (i32.const 55))  ;; '7'
            (local.set $pos (i32.sub (local.get $pos) (i32.const 1)))
            (i32.store8 (local.get $pos) (i32.const 52))  ;; '4'
            (local.set $pos (i32.sub (local.get $pos) (i32.const 1)))
            (i32.store8 (local.get $pos) (i32.const 49))  ;; '1'
            (local.set $pos (i32.sub (local.get $pos) (i32.const 1)))
            (i32.store8 (local.get $pos) (i32.const 50))  ;; '2'
            (local.set $pos (i32.sub (local.get $pos) (i32.const 1)))
            (i32.store8 (local.get $pos) (i32.const 45))  ;; '-'
            (local.set $pos (i32.sub (local.get $pos) (i32.const 1)))
          )
          (else
            ;; Regular negative number
            (local.set $value (i32.sub (i32.const 0) (local.get $value)))
            (call $convert_digits (local.get $value))
            (local.set $pos (global.get $pos_result))
            ;; Add minus sign
            (i32.store8 (local.get $pos) (i32.const 45)) ;; '-'
            (local.set $pos (i32.sub (local.get $pos) (i32.const 1)))
          )
        )
      )
      (else
        ;; Positive number
        (call $convert_digits (local.get $value))
        (local.set $pos (global.get $pos_result))
      )
    )

    ;; Calculate start position and length
    (local.set $start_pos (i32.add (local.get $pos) (i32.const 1)))
    (local.set $len (i32.sub
      (i32.add (global.get $buffer_start) (i32.const 64))
      (local.get $start_pos)))

    ;; Add newline
    (i32.store8 (i32.add (local.get $start_pos) (local.get $len)) (i32.const 10)) ;; '\n'
    (local.set $len (i32.add (local.get $len) (i32.const 1)))

    ;; Write to stdout
    (call $write_to_stdout (local.get $start_pos) (local.get $len))
  )

  ;; EXPORTED LIBRARY FUNCTION: print_string
  ;; Prints a null-terminated string to stdout with newline
  (func (export "print_string") (param $ptr i32)
    (local $len i32)
    (local $current i32)

    ;; Find string length
    (local.set $current (local.get $ptr))
    (local.set $len (i32.const 0))

    (loop $count_loop
      (if (i32.ne (i32.load8_u (local.get $current)) (i32.const 0))
        (then
          (local.set $len (i32.add (local.get $len) (i32.const 1)))
          (local.set $current (i32.add (local.get $current) (i32.const 1)))
          (br $count_loop)
        )
      )
    )

    ;; Add newline
    (i32.store8 (local.get $current) (i32.const 10)) ;; '\n'
    (local.set $len (i32.add (local.get $len) (i32.const 1)))

    ;; Write to stdout
    (call $write_to_stdout (local.get $ptr) (local.get $len))

    ;; Restore null terminator (replace newline with null)
    (i32.store8 (local.get $current) (i32.const 0))
  )

  ;; Internal helper: convert digits
  (func $convert_digits (param $value i32)
    (local $pos i32)
    (local $digit i32)

    (local.set $pos (i32.add (global.get $buffer_start) (i32.const 63)))

    ;; Handle zero case
    (if (i32.eqz (local.get $value))
      (then
        (i32.store8 (local.get $pos) (i32.const 48)) ;; '0'
        (local.set $pos (i32.sub (local.get $pos) (i32.const 1)))
      )
      (else
        ;; Convert digits from right to left
        (loop $digit_loop
          (local.set $digit (i32.rem_u (local.get $value) (i32.const 10)))
          (local.set $value (i32.div_u (local.get $value) (i32.const 10)))
          (i32.store8 (local.get $pos)
            (i32.add (local.get $digit) (i32.const 48))) ;; ASCII '0' + digit
          (local.set $pos (i32.sub (local.get $pos) (i32.const 1)))
          (br_if $digit_loop (i32.ne (local.get $value) (i32.const 0)))
        )
      )
    )

    (global.set $pos_result (local.get $pos))
  )

  ;; Internal helper: write to stdout via WASI
  (func $write_to_stdout (param $ptr i32) (param $len i32)
    ;; Set up iovec structure
    (i32.store (global.get $iovec_start) (local.get $ptr))        ;; iov_base
    (i32.store (i32.add (global.get $iovec_start) (i32.const 4)) (local.get $len)) ;; iov_len

    ;; Call fd_write: fd=1 (stdout), iovs=iovec_start, iovs_len=1, nwritten=nwritten_ptr
    (drop (call $fd_write
      (i32.const 1)                ;; stdout file descriptor
      (global.get $iovec_start)    ;; pointer to iovec array
      (i32.const 1)                ;; number of iovecs
      (global.get $nwritten_ptr)   ;; pointer to store number of bytes written
    ))
  )
)

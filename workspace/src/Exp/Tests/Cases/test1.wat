(module
   (import "runtime" "print_int" (func $print_int (param i32)))
   (import "runtime" "memory" (memory 1))
   (func $main
       i32.const 1
       i32.const 3
       i32.const 5
       i32.mul
       i32.add
       i32.const 6
       i32.add
       call $print_int)
   (export "main" (func $main))
   (start $main))
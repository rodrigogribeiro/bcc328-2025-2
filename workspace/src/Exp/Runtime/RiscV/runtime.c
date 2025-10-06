#include <stdio.h>
#include <stdlib.h>
#include "runtime.h"

void print_int(int value) {
    printf("%d\n", value);
}

void exit_program(int status) {
    exit(status);
}

#include <stdio.h>
#include <stdlib.h>
#include "runtime.h"

void print_int(int value) {
    printf("%d\n", value);
}

int read_int(int *value) {
    scanf("%d", value);
}

void exit_program(int status) {
    exit(status);
}

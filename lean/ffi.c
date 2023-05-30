// #include <lean/lean.h>
#include <stdbool.h>
#include <stdio.h>
#include "./ffi.h"

int main(void) {
    bool v = bern(0.7);
    printf("in c, got: %d", v);
    return 0;
}

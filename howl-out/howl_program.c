#include <stdio.h>
#include <stdint.h>
#include <stdbool.h>
#include <string.h>
#include <stddef.h>
#include <stdlib.h>

typedef float howl_f32_t;
typedef double howl_f64_t;

// ============================================================================
// Optional MyStruct Implementation
// ============================================================================

typedef struct Optional_MyStruct_t {
    MyStruct value;
    int32_t is_some; // 1 if has value, -1 if none
} Optional_MyStruct_t;

Optional_MyStruct_t Optional_MyStruct_t_some(MyStruct value) {
    Optional_MyStruct_t result = {.value = value, .is_some = 1};
    return result;
}

Optional_MyStruct_t Optional_MyStruct_t_none() {
    Optional_MyStruct_t result = {.value = {}, .is_some = -1};
    return result;
}


int main() {
    int32_t a = 1;
    int32_t b = 2;
    int32_t c = (a + b);
    printf("Sum: %d", c);
    return 0;
    return 0;
}




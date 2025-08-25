#include <stdio.h>
#include <stdlib.h>
#include <stdbool.h>
#include <stdint.h>

typedef int8_t i8;
typedef int16_t i16;
typedef int32_t i32;
typedef int64_t i64;
typedef uint8_t u8;
typedef uint16_t u16;
typedef uint32_t u32;
typedef uint64_t u64;
typedef float f32;
typedef double f64;

typedef struct {
    i64 error_code;
    i32 payload;
} MyError_i32_Union;

/* IR contains 25 nodes */
MyError_i32_Union divide(i32 a, i32 b) {
    i64 v0 = 0;
    i64 v1 = b == v0;
    MyError_i32_Union v2;
    if (v1) {
        v2 = (MyError_i32_Union){ .error_code = 2487539981, .payload = -1 };
    } else {
        if (b != 0) {
            v2 = (MyError_i32_Union){ .error_code = 0, .payload = a / b };
        } else {
            v2 = (MyError_i32_Union){ .error_code = 2487539981, .payload = -1 };
        }
    }
    return v2;
}

int main() {
    i64 v3 = 10;
    i64 v4 = 2;
    MyError_i32_Union v5 = divide(v3, v4);
    // Try expression
    if ((v5).error_code != 0) {
        printf("Error: %lld\n", (v5).error_code);
        return 1;
    }
    i32 v6 = (v5).payload;
    char* v7 = "builtin_print";
    char* v8 = "Result of division: {}\n";
    printf("Result of division: %lld\n", v6);
    i64 v9 = 0; /* printf return value */
    i64 v10 = 10;
    i64 v11 = 0;
    MyError_i32_Union v12 = divide(v10, v11);
    // Try expression
    if ((v12).error_code != 0) {
        printf("Error: %lld\n", (v12).error_code);
        return 1;
    }
    i32 v13 = (v12).payload;
    char* v14 = "builtin_print";
    char* v15 = "Result of division 2: {}\n";
    printf("Result of division 2: %lld\n", v13);
    i64 v16 = 0; /* printf return value */
    return 0;
}


#include <stdio.h>
#include <stdint.h>
#include <stdbool.h>
#include <string.h>
#include <stddef.h>
#include <stdlib.h>

typedef float howl_f32_t;
typedef double howl_f64_t;

// ============================================================================
// Error Set MyError Implementation
// ============================================================================

typedef enum {
    MyError_SUCCESS = 0,
    MyError_DivisionByZero = -1,
    MyError_UnknownError = -2,
    MyError_InvalidValue = -3
} MyError_t;

// ============================================================================
// Enum MyEnum Implementation
// ============================================================================

typedef enum {
    MyEnum_Value1,
    MyEnum_Value2,
    MyEnum_Value3
} MyEnum;

// ============================================================================
// Struct MyStruct Implementation
// ============================================================================

typedef struct MyStruct {
    int32_t field1;
    howl_f64_t field2;
} MyStruct;

MyStruct MyStruct_init(int32_t field1, howl_f64_t field2) {
    MyStruct result = {.field1 = field1, .field2 = field2};
    return result;
}

// ============================================================================
// Error Union MyError_MyStruct_ErrorUnion Implementation
// ============================================================================

typedef struct MyError_MyStruct_ErrorUnion {
    MyError_t error;
    MyStruct payload;
} MyError_MyStruct_ErrorUnion;

// ============================================================================
// Error Union MyError_i32_ErrorUnion Implementation
// ============================================================================

typedef struct MyError_i32_ErrorUnion {
    MyError_t error;
    int32_t payload;
} MyError_i32_ErrorUnion;

MyStruct MyStruct_init(int32_t field1, howl_f64_t field2);

MyError_MyStruct_ErrorUnion createMyStruct(int32_t a, howl_f64_t b) {
    if (a < 0) {
        return (MyError_MyStruct_ErrorUnion){.error = MyError_InvalidValue, .payload = 0};
    } else if (true) {
        return (MyError_MyStruct_ErrorUnion){.error = MyError_SUCCESS, .payload = MyStruct_init(a, b)};
    }
}



MyError_i32_ErrorUnion divide(int32_t a, int32_t b) {
    {
        if (b == 0) {
            MyError_i32_ErrorUnion result = {.error = MyError_DivisionByZero, .payload = 0};
            return result;
        } else {
            MyError_i32_ErrorUnion result = {.error = MyError_SUCCESS, .payload = (a / b)};
            return result;
        }
    }
}



int main() {
    printf("Starting division...\n");
    int32_t result = ({ MyError_i32_ErrorUnion _temp = divide(10, 2); if (_temp.error < 0) exit(1); _temp.payload; });
    printf("Result of division: %d\n", result);
    printf("Enum value: %d\n", MyEnum_Value1);
    MyStruct my_struct = ({ MyError_MyStruct_ErrorUnion _temp = createMyStruct(42, 3.14f); if (_temp.error < 0) exit(1); _temp.payload; });
    printf("MyStruct: %d, field2: %f}}\n", my_struct.field1, my_struct.field2);
    MyStruct my_struct2 = ({ MyError_MyStruct_ErrorUnion _temp = createMyStruct((-1), 2.71f); if (_temp.error < 0) exit(1); _temp.payload; });
    printf("MyStruct2: %d, field2: %f}}\n", my_struct2.field1, my_struct2.field2);
    int32_t *simple_gc_array = malloc(3 * sizeof(int32_t));
    int32_t first_element = simple_gc_array[0];
    printf("GC Array - First element: %d\n", first_element);
    MyStruct *people = malloc(2 * sizeof(MyStruct));
    MyStruct first_person = people[0];
    printf("GC Struct Array - First person field1: %d\n", first_person.field1);
    int32_t result2 = ({ MyError_i32_ErrorUnion _temp = divide(10, 0); if (_temp.error < 0) exit(1); _temp.payload; });
    printf("This line will not be reached due to error.\n");
    return 0;
}




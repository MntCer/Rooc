#include <stdio.h>
#include <stdbool.h>

typedef struct {
    int print_str();
} testTraitAgain;

typedef struct {
    int print_str(void);
    bool print_bool(void);
    float print_float(void);
    char* print_text(void);
    void no_return(void);
    int random(int x);
    int twoInt(int x, int y);
    float twoFlo(float x, float y);
    float diffInput(char* x, bool y);
    bool testEqual(int x, float y, bool z);
    char* intToStr(x:int);
} testTrait;
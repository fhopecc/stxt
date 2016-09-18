//string_literal.c
#include <stdio.h>

char* getstr() {
    return "string literal";
}

int main(void) {
    char* str;
    str = getstr();
    printf("%s\n", str);
}


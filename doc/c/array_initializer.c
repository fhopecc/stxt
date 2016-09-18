//array_initializer.c
#include <stdio.h>
#include <stdlib.h>

int* getints1() {
    return (int[4]){1, 2, 3, 4};
}

int* getints2() {
    int* is = (int*)malloc(sizeof(int)*4);
    is[0]=1;is[1]=2;is[2]=3;is[3]=4;
    return is;
}

int main(void) {
    int i, *is;
    is=getints1();
    printf("%s\n", "list getints1 array");
    for(i=0;i<4;i++)
        printf("%i\n", is[i]);
    printf("%s\n", "list getints2 array");
    is=getints2();
    for(i=0;i<4;i++)
        printf("%i\n", is[i]);
}


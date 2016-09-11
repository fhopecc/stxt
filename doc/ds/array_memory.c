#include <stdio.h>

void main(void) {
    int i;
    int a[5] = {2, 3, 5, 7, 11};
    for(i=0;i<5;i++)
       printf("%p %i\n", &a[i], a[i]);
}

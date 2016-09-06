#include "ku.h"
#include <stdio.h>

unsigned int hash(str s) {
   unsigned int seed = 131; /* 31 131 1313 13131 131313 etc.. */
   unsigned int hash = 0;
   unsigned int i    = 0;
   unsigned int len = wcslen(s);
   for(i = 0; i < len; s++, i++)
   {
      hash = (hash * seed) + (*s);
   }
   return hash % MAX_XIANG_NUM ;
}

int add_yuan(str ming) {
    int h = hash(ming);
    xiang x = {.xin = YUAN, .zhi=(void *)ming};
    ku[h] = x;
}

int get_yuan(str ming) {
    int h = hash(ming);
    xiang x = ku[h];
    printf("xiang is %d", x.xin);
    if(wcscmp(ming, (str)(x.zhi))==0)
        return 1;
    else
        return 0;
}

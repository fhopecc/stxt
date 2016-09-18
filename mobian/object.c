#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <assert.h>
#include "mobian.h"

// 建立項
xiang newshu(double shu) {
    xiang x = (xiang)malloc(sizeof(struct _xiang));
    x->lei = SHU;
    x->zhi = malloc(sizeof(double));
    *(double *)(x->zhi) = shu;
    return x;
}

xiang newfu(str fu) {
    xiang x = (xiang)malloc(sizeof(struct _xiang));
    x->lei = FU;
    x->zhi = fu; 
    return x;
}

xiang newbian(str bian) {
    xiang x = (xiang)malloc(sizeof(struct _xiang));
    x->lei = BIAN;
    x->zhi = bian; 
    return x;
} 

xiang newzu(str ming, size_t zhi, xiang* can) {
    int i;
    xiang x = (xiang)malloc(sizeof(struct _xiang));
    x->lei = ZU;
    zu z = (zu)malloc(sizeof(struct _zu)); 
    z->ming = ming;
    z->zhi = zhi;
    // 複製參，參為陣列，傳入的參生命期若是 automatic，
    // 則在不同區塊時，其生命過期記憶體區塊會不可用。
    z->can = malloc(zhi*sizeof(struct _xiang));
    for(i=0;i<zhi;i++)
        z->can[i]=can[i];
    x->zhi = (void *)z; 
    return x;
}

// 建立句
ju newshi(size_t chang, xiang* lie) {
    int i;
    ju j = (ju)malloc(sizeof(struct _ju));
    j->chang = chang;
    // 複製列，不然傳入的列若是automatic 變數，
    // 其變數生命周期失效，記憶體區塊會不可用。
    j->lie = (xiang*)malloc(chang*sizeof(struct _xiang));
    for(i=0;i<chang;i++)
        j->lie[i] = lie[i];
    j->xin = SHI;
    return j;
}
 
ju newfa(size_t chang, xiang* lie) {
    ju j = newshi(chang, lie);
    j->xin = FA;
    return j;
}

double getshu(xiang x) {
    assert(x->lei == SHU);
    return *(double *)(x->zhi);
}

str getming(xiang x) {
    assert(x->lei == FU || x->lei==BIAN || x->lei==ZU);
    if(x->lei==ZU) {
        zu z = (zu)x->zhi;
        return (str)z->ming;
    } else
        return (str)x->zhi;
}

size_t getzhi(xiang x) {
    assert(x->lei == ZU);
    zu z = (zu)x->zhi;
    return z->zhi;
}

xiang* getcan(xiang x) {
    assert(x->lei == ZU);
    zu z = (zu)x->zhi;
    return z->can;
}

// 求出雜湊值
size_t strhash(str s) {
   size_t seed = 131; /* 31 131 1313 13131 131313 etc.. */
   size_t hash = 0;
   size_t i    = 0;
   size_t len  = wcslen(s); 
   for(i = 0; i < len; s++, i++)
      hash = (hash * seed) + (*s);
   return hash;
}

size_t xianghash(xiang x) {
    size_t i, h;
    switch(x->lei) {
        case SHU:
            return (size_t)getshu(x);
        case FU:
        case BIAN:
            return strhash(getming(x));
        case ZU:
            h = strhash(getming(x)) + getzhi(x);
            for(i=0;i<getzhi(x);i++)
                h+=xianghash(getcan(x)[i]);
            return h;
    }
    assert(-1);
}

// 存取句屬性
double getchang(ju j) {
    return j->chang;
}

xiang* getlie(ju j) {
    return j->lie;
}

xin getxin(ju j) {
    return j->xin;
}

size_t juhash(ju j) {
    size_t i, h;
    switch(j->xin) {
        case SHI:
        case FA:
            h = getxin(j) + getchang(j);
            for(i=0;i<getchang(j);i++)
                h+=xianghash(getlie(j)[i]);
            return h;
    }
    assert(-1);
}

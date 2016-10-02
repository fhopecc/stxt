#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <assert.h>
#include "mobian.h"

/* 庫 */
jus ku;

// 建立項
xiang newshu(double shu) {
    xiang x = (xiang)malloc(sizeof(struct _xiang));
    x->lei = SHU;
    x->data = malloc(sizeof(double));
    *(double *)(x->data) = shu;
    return x;
}

xiang newfu(str fu) {
    xiang x = (xiang)malloc(sizeof(struct _xiang));
    x->lei = FU;
    x->data = fu; 
    return x;
}

/* 變 */
xiang newbian(str bian) {
    xiang x = (xiang)malloc(sizeof(struct _xiang));
    x->lei = BIAN;
    x->data = bian; 
    x->zhi = NULL; //合一.2.1.1.
    return x;
} 

xiang newzu(str ming, size_t wei, xiang* can) {
    int i;
    xiang x = (xiang)malloc(sizeof(struct _xiang));
    x->lei = ZU;
    zu z = (zu)malloc(sizeof(struct _zu)); 
    z->ming = ming;
    z->wei = wei;
    // 複製參，參為陣列，傳入的參生命期若是 automatic，
    // 則在不同區塊時，其生命過期記憶體區塊會不可用。
    z->can = malloc(wei*sizeof(struct _xiang));
    for(i=0;i<wei;i++)
        z->can[i]=can[i];
    x->data = (void *)z; 
    return x;
}

/* 句 */
ju newshi(xiang x) {
    xiang xs[1]={x};
    ju j = newwen(1, xs);
    j->xin = SHI;
    return j;
}

ju newwen(size_t chang, xiangs xs) {
    int i;
    ju j = (ju)malloc(sizeof(struct _ju));
    j->chang = chang;
    // 複製列，不然傳入的列若是automatic 變數，
    // 其變數生命周期失效，記憶體區塊會不可用。
    j->lie = (xiang*)malloc(chang*sizeof(struct _xiang));
    for(i=0;i<chang;i++)
        j->lie[i] = xs[i];
    j->xin = WEN;
    return j;
}
 
ju newfa(xiang tou, size_t chang, xiangs ti) {
    int i;
    xiang lie[chang+1]; 
    lie[0] = tou; //把頭加進去
    for(i=0;i<chang;i++) {
        lie[i+1]=ti[i];
    }
    ju j = newwen(chang, lie);
    j->xin = FA;
    return j;
}

xiang gettou(ju j) {
    assert(j->xin==SHI || j->xin==FA);
    return j->lie[0];
}

size_t getchang(ju j) {
    return j->chang;
}

xiang getzi(ju j, size_t i) {
    switch(j->xin) {
    case FA:
        return j->lie[i+1];
    case WEN:
        return j->lie[i];
    }
    assert(false); // xin must be in FA or WEN
}

xiang* getlie(ju j) {
    return j->lie;
}

xin getxin(ju j) {
    return j->xin;
}

double getshu(xiang x) {
    assert(x->lei == SHU);
    return *(double *)(x->data);
}

str getming(xiang x) {
    assert(x->lei == FU || x->lei==BIAN || x->lei==ZU);
    if(x->lei==ZU) {
        zu z = (zu)x->data;
        return (str)z->ming;
    } else
        return (str)x->data;
}

//合一.2.1.
xiang getzhi(xiang b) {
    assert(b->lei==BIAN);
    return b->zhi;
}

//合一.2.1.1.
bool isbangdin(xiang b) {
    assert(b->lei==BIAN);
    return getzhi(b) != NULL;
}

void bangdin(xiang b, xiang z) {
    assert(b->lei==BIAN);
    assert(!isbangdin(b));
    b -> zhi = z;
    if(z->lei==BIAN) //2.2.
        z->zhi=b;
}

void unbangdin(xiang b) {
    assert(b->lei==BIAN);
    b -> zhi = NULL;
}

//合一.2.1.1.

size_t getwei(xiang x) {
    assert(x->lei == ZU);
    zu z = (zu)x->data;
    return z->wei;
}

xiang* getcan(xiang x) {
    assert(x->lei == ZU);
    zu z = (zu)x->data;
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
            h = strhash(getming(x)) + getwei(x);
            for(i=0;i<getwei(x);i++)
                h+=xianghash(getcan(x)[i]);
            return h;
    }
    assert(-1);
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

// 合一
bool ismingeq(xiang x1, xiang x2) {
    assert(x1->lei==FU || x1->lei==BIAN || x1->lei==ZU);
    assert(x2->lei==FU || x2->lei==BIAN || x2->lei==ZU);
    return wcscmp(getming(x1), getming(x2)) == 0;
}
bool unify_xiang(xiang x1, xiang x2) {
    int i;
    // 0.合一的交換性
    // 合一的交換性使 x1 及 x2 的順序不重要。
    // 只列舉可合一的情形，其它情形均不可合一
        
    switch(x1->lei) {
    case SHU://0.1
        switch(x2->lei) {
        case SHU:
            return getshu(x1)==getshu(x2);
        case BIAN: //2.1
            return unify_xiang(x2, x1); 
        }
        break;
    case FU:
        switch(x2->lei) { 
        case FU:
            return wcscmp(getming(x1), getming(x2)) == 0;
        case BIAN: //2.1
            return unify_xiang(x2, x1); 
        }
        break;
    case BIAN: 
        if(!isbangdin(x1)) { //2.1.
            bangdin(x1, x2);
            return true;
        } else //2.3.
            return unify_xiang(getzhi(x1), x2);
        break;
    case ZU:
        if( x2->lei==ZU 
         && ismingeq(x1, x2)
         && getwei(x1) == getwei(x2)) {
            for(i=0;i<getwei(x1);i++) {
                if(!unify_xiang(getcan(x1)[i], getcan(x2)[i]))
                   return false; 
            }
            return true;
        }
    }
    return false;
}


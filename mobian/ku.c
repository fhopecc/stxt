/* ku.c
 * 
 * 庫的目標是讓使用都能簡單的以名去取出元和函，
 * 類似於符號表，故以雜湊表實作。
 *
 * 為了節省空間，不會儲存相同的元或函。
 *
 * 為求不受預定大小的限制，其儲存以動態陣列實作，
 * 因為整個程式啟動只會有一個庫，
 * 所以並未有釋放記憶體的動作。
 * 考慮簡化記憶體的管理，
 * 所以碰撞處理不採鏈結法，採取位址展開法。
 * 
 * 稜於花蓮美崙
 * 20160911 週日
 */
#include "ku.h"
#include <stdbool.h>
#include <stdlib.h>
#include <assert.h>

#define INITIAL_SIZE 1024 /* 陣列預設容量 */
#define EXTENT_SIZE 2048 /* 陣列容量擴充單位 */

size_t size;     // 目前陣列容量
size_t used;     // 已配置的儲存格數
xiang * ku=NULL;  // 動態陣列用來儲存項 

void initialize_ku() {
    ku = (xiang*)malloc(INITIAL_SIZE * sizeof(xiang)); 
    if(ku==NULL) raise_error(ALLOCATION_MEMROY_FAILED);
    size = INITIAL_SIZE;
    int i;
    for(i=0;i<INITIAL_SIZE;i++)
        ku[i]=NULL;
    used = 0;
}

/* 陣列擴充 */
void extend_ku() {
    int i;
    xiang *old_ku = ku;
    size_t old_size = size;
    
    //改變大小並重設庫
    size += EXTENT_SIZE;
    initialize_ku();

    // 複製陣列至新陣列，因為雜湊函數與容量有關，
    // 所以容量改變，需全部重新加入，並將舊庫記憶體釋放。
    for(i=0; i<old_size; i++) {
        if(old_ku[i] != NULL) 
            add(xiang);
    }
    free(old_ku); //釋放舊庫記憶體空間
}

unsigned int wei_hash(str ming, int zhi) {
    return hash(ming) + zhi;
}

// 可以定義的項含元、組及式。
unsigned int get_hash(xiang x) {
    assert(x!=NULL);
    unsigned int h;
    xiang* can, wei;
    xiang shou;
    switch(x.xin) {
        case SHU:
            return get_shu(x) % size;
        case YUAN: 
            return hash(get_ming(x));
        case ZU:
            h = hash(get_ming(x) + get_zhi(x));
            can = get_can(x);
            for(i=0; i<get_zhi(x); i++)
                h+=get_hash(can[i]);     
            return h;
        case FA:
            h = get_hash(get_shou(x));
            wei = get_wei(x);
            for(i=0; i<get_chang(x); i++)
                h+=get_hash(wei[i]);
    }
    assert(1);
}

// 判斷索引 i 是否為可用位址
bool available(int i) {
    return (ku[i]==NULL);
}

/* 找出可用位址，以位址展開法實作 */
int probe(int i) {
    if(available(i))
        return i;
    else 
        return probe((i+1) % size);
}

/* 元的本質就是字串 */
bool add_yuan(str ming) {
    if(ku==NULL) initialize_ku();
    //不可加入重覆的元，以節省空間。
    if(has_yuan(ming)) {
        raise_exception(ADD_EXISTING_XIANG);
        return false;
    }

    //若負載因子大於 0.75，擴展槽，避免存取效能降低。
    if(used/size > 0.75) extend_ku(); 

    int i = probe(hash(ming));
    ku[i] = malloc(sizeof(struct _xiang));
    ku[i]->xin = YUAN;
    ku[i]->zhi.data = (void *)ming;
    used += 1;
    return true;
}

xiang get_yuan(str ming) {
    if(ku == NULL) return NULL;
    int i = -1; // -1 標示尚未取雜湊值
    do {
        i = i==-1 ? hash(ming):(i+1)%size;
        if(ku[i] == NULL) return NULL;
        if(ku[i]->xin == YUAN)
            if(wcscmp(ming, get_ming(ku[i])==0)) return ku[i];
    } while(1==1);
    return NULL;
}

/* 比較參組，0 表參組相等 */
int cmp_canzu(int zhi, xiang* c1, xiang* c2) {
    int i;
    int r; // 儲存陣列所有元素比較的結果，
           // 利用結果絕對值之加總是否為零可得整個陣列是否相等
    for(i=0;i<zhi;i++)
        r += cmp_xiang(c1[i], c2[i]);
    return r;
}

wei get_wei(xiang x) {
    if(x == NULL) return NULL;
    if(x->xin != WEI) return NULL;
    return (wei)x->zhi.data; 
}

bool has_can(xiang x, xiang* c) {
    assert(x!=NULL);
    assert(x->xin==WEI);
    wei w = get_wei(x);
    canlian canlian = w->canlian;
    while(canlian==NULL)
        if(cmp_canzu(w->zhi, c, canlian->canzu)!=0) return true;
    return false;
}

xiang get_shi(str ming, int zhi, xiang* can) {
    if(ku == NULL) return NULL;
    int i = -1; // -1 標示尚未取雜湊值
    do {
        i = i==-1 ? wei_hash(ming, zhi):(i+1)%size;
        if(ku[i] == NULL) return NULL;
    } while(ku[i]->xin != WEI 
         || get_zhi(ku[i]) != zhi
         ||(wcscmp(ming, get_ming(ku[i])))!=0);

    // 測試是否包含參
    return has_can(ku[i], can) ? ku[i] : NULL;
}

int get_zhi(xiang x) {
    assert(x!=NULL);
    assert(x->xin==WEI);
    wei w = (wei)x->zhi.data; 
    return w->zhi;
}

bool has_yuan(str ming) {
    return (get_yuan(ming) != NULL);
}

bool has_shi(str ming, int zhi, xiang* can) {
    int i = wei_hash(ming, zhi);
    xiang x = ku[i];
    if(x->xin != WEI) return false; /* 只能執行在謂上 */
    //wei w = (wei)x.zhi.data;
    //canlian_t * cs = w.canlian;
    return true;
}

bool add_shi(str ming, int zhi, xiang* xiangzu) {
    if(ku==NULL) initialize_ku();

    //不可加入重覆的實，以節省空間。
    if(has_shi(ming, zhi, xiangzu)) {
        raise_exception(ADD_EXISTING_XIANG);
        return false;
    }

    //若負載因子大於 0.75，擴展槽，避免存取效能降低。
    if(used/size > 0.75) extend_ku(); 

    int i = probe(hash(ming));
    ku[i]->xin = YUAN;
    ku[i]->zhi.data = (void *)ming;
    used += 1;
    return true;
}

/* 比較 2 個項，相同傳回0，其它情形傳回 n。 */
int cmp_xiang(xiang x1, xiang x2) {
    if(x1==NULL || x2==NULL) return 1;
    if(x1->xin != x2->xin) return 1; // 型不同即不相同
    wei w1, w2;
    switch(x1->xin) {
        case YUAN: 
            return abs(wcscmp(get_ming(x1), get_ming(x2)));
        case WEI:
            w1= get_wei(x1);
            w2= get_wei(x2);
            if(wcscmp(w1->ming, w2->ming)!=0) return 1;
            if(w1->zhi != w2->zhi) return 1;
            if(w1->canlian == w2->canlian) return 1;
        default:
            return 1;
    }
}

xiang get_shu(double s) {
    xiang x = malloc(sizeof(struct _xiang));
    x->xin=s;
    x->zhi.shu=s;
    return x;
}
/* 取項名 */
str get_ming(xiang x) {
    if(x->xin == SHU) return NULL; // 數沒有名
    wei w;
    switch(x->xin) {
        case YUAN:
            return (str)x->zhi.data;
        case WEI:
            w = (wei)x->zhi.data;
            return w->ming;
        default:
            return NULL;
    }
}

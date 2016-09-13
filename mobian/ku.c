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

#define INITIAL_SIZE 1024 /* 陣列預設容量 */
#define EXTENT_SIZE 2048 /* 陣列容量擴充單位 */

size_t size;     // 目前陣列容量
size_t used;     // 已配置的儲存格數
xiang *ku=NULL;  // 動態陣列用來儲存項 

void initialize_ku() {
    ku = (xiang*)malloc(INITIAL_SIZE * sizeof(xiang)); 
    if(ku==NULL) error(ALLOCATION_MEMROY_FAILED);
    size = INITIAL_SIZE;
    used = 0;
}

/* 陣列擴充 */
void extend_ku() {
    int i;
    xiang *old_ku = ku;
    size_t old_size = size;
    size += EXTENT_SIZE;
    ku = (xiang*)malloc(size* sizeof(xiang));
    if(ku==NULL) error(ALLOCATION_MEMROY_FAILED);
    // 複製陣列至新陣列，因為雜湊函數與容量有關，
    // 所以容量改變，需全部重新加入，並將舊庫記憶體釋放。
    for(i=0; i<old_size; i++) {
        switch(old_ku[i].xin) {
            case YUAN:
                add_yuan((str)old_ku[i].zhi.data);
                break;
            default:
                break;
        }
    }
    free(old_ku); //釋放舊庫記憶體空間
}

unsigned int hash(str s) {
   unsigned int seed = 131; /* 31 131 1313 13131 131313 etc.. */
   unsigned int hash = 0;
   unsigned int i    = 0;
   unsigned int len = wcslen(s); 
   for(i = 0; i < len; s++, i++)
      hash = (hash * seed) + (*s);
   return hash % size;
}

// 因為我們是由謂的名及枝來查詢，
// 故謂的雜湊值為名的雜湊值加上枝
unsigned int hash_wei(str ming, int zhi) {
    return hash(ming) + zhi;
}

// 判斷索引 i 是否為可用位址
bool available(int i) {
    return (ku[i].xin==0);
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
        exception(ADD_EXISTING_XIANG);
        return false;
    }

    //若負載因子大於 0.75，擴展槽，避免存取效能降低。
    if(used/size > 0.75) extend_ku(); 

    int i = probe(hash(ming));
    ku[i].xin = YUAN;
    ku[i].zhi.data = (void *)ming;
    used += 1;
    return true;
}

bool has_yuan(str ming) {
    int i = hash(ming);
    if(ku[i].xin != YUAN) return false;
    while(wcscmp(ming, (str)(ku[i].zhi.data))!=0) {
        i = (i+1) % size; 
        if(ku[i].xin == KONG) return false;
    }
    return true;
}

bool add_shi(str ming, int zhi, xiang* can) {
    if(ku==NULL) initialize_ku();

    //不可加入重覆的實，以節省空間。
    if(has_shi(ming, zhi, can)) {
        exception(ADD_EXISTING_XIANG);
        return false;
    }

    //若負載因子大於 0.75，擴展槽，避免存取效能降低。
    if(used/size > 0.75) extend_ku(); 

    int i = probe(hash(ming));
    ku[i].xin = YUAN;
    ku[i].zhi.data = (void *)ming;
    used += 1;
    return true;
}

/* 比較項 */
int cmp_xiang(xiang x1, xiang x2) {
    if(x1.xin != x2.xin) return 1; // 型不同即不相同
    switch(x1.xin) {
        case YUAN: 
            return 0;
        default:
            return 0;
    }
}

/* 取項名 */
str get_ming(xiang x) {
    if(x.xin == KONG) return NULL; // 數及空項均沒有名
    if(x.xin == SHU) return NULL;
    wei w;
    switch(x.xin) {
        case YUAN:
            return (str)x.zhi.data;
        case WEI:
            w = (wei)x.zhi.data;
            return w.ming;
        default:
            return NULL;
    }
}

/* 比較參組，0 表參組相等 */
int cmp_canzu(int zhi, xiang* c1, xiang* c2) {
    int i;
    int r; // 儲存陣列元性比較的結果，
           // 利用結果絕對值之加總是否為零可得整個陣列是否相等
    for(i=0;i<zhi;i++)
        r += cmp_xiang(c1[i], c2[i]);
    return r;
}

bool has_shi(str ming, int zhi, xiang* canzu) {
    int i = wei_hash(ming, zhi);
    xiang x = ku[i];
    if(x.xin != WEI) return false; /* 只能執行在謂上 */
    wei w = (wei)x.zhi.data;
    canlian_t * cs = w.canlian;
    return true;
}

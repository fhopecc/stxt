/* ku.c
 * 
 * 庫本質是符號表，故以雜湊表實作，
 * 為求彈性，其儲存為動態陣列，
 * 因為整個程式啟動只會有一個庫，
 * 所以並未有釋放記憶體的動作。
 * 考慮簡化記憶體的管理，
 * 所以碰撞處理不採鏈結法，採取位址展開法。
 * 
 * 稜於花蓮美崙
 * 20160911 週日
 */
#include "ku.h"
#include "mobian.h"
#include <stdbool.h>
#include <stdlib.h>

#define INITIAL_SIZE 1024 /* 陣列預設容量 */
#define EXTENT_SIZE 2048 /* 陣列容量擴充單位 */

size_t size;        // 目前陣列容量
size_t used;        // 已配置的儲存格數
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
    // 所以容量改變，需全部重新加入。
    for(i=0; i<old_size; i++) {
        switch(old_ku[i].xin) {
            case YUAN:
                add_yuan((str)old_ku[i].zhi);
                break;
            default:
                break;
        }
    }
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

int add_yuan(str ming) {
    if(ku==NULL) initialize_ku();

    //若負載因子大於 0.75，擴展槽，避免存取效能降低。
    if(used/size > 0.75) extend_ku(); 

    int i = probe(hash(ming));
    ku[i].xin = YUAN;
    ku[i].zhi = (void *)ming;
    used += 1;
}

bool has_yuan(str ming) {
    int i = hash(ming);
    if(ku[i].xin == KONG) return false;
    while(wcscmp(ming, (str)(ku[i].zhi))!=0) {
        i = (i+1) % size; 
        if(ku[i].xin == KONG) return false;
    }
    return true;
}

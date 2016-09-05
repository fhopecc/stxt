#include "dataobject.h"

#define MAX_XIANG_NUM 1024 /* 知識庫最大項數 */

extern xiang ku[MAX_XIANG_NUM]; /* 初始庫，庫用來儲存項 */ 

int hash(xiang x); /* 產出項的散列值，其範圍為知識庫最大項數 */

int add_yuan(str ming); /* 將項加入至知識庫 */

int get_yuan(str ming); /* 以名判斷元是否存在，0 為不存在，1 為存在 */ 

/* 以名找出所有的謂，並將謂存在 ws 位置  */ 
int get_wei(str ming, wei* ws); 

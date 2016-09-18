/* ku.h
 * 
 * 庫用來儲存所有墨辨使用者定義的項目。
 *
 * 關於定義內容的操作包含增加元、
 * 以名取出元。
 *
 * 關於統計資料的存取包含項的數目。
 * 
 * 稜於花蓮美崙
 * 20160911 週日
 */
#include "mobian.h"

#define ALLOCATION_MEMROY_FAILED 1 /* 記憶體配置失敗 */

/* 產出項的散列值，其範圍為知識庫最大項數 */
unsigned int hash(str ming); 

/* 以名找出所有的謂，並將謂存在 ws 位置  */ 
/* int get_wei(str ming, wei* ws); */

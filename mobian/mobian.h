/* mobian.h
 *
 * 墨辨的基礎類型定義、錯誤處理
 *  
 * 稜於花蓮美崙
 * 20160819 週五
 */
#include <wchar.h>


typedef wchar_t * str;

typedef enum {
    KONG=0,//項結構初始時，型會設成0，因此0保留給空
    YUAN=1,
    HAN=2, 
    SHU=3
} xin; //型

typedef struct {
    xin xin;    //型
    zhi zhi;  //值
} xiang; //項

typedef struct {
    str wei;     // 謂
    int zhi;     // 枝
    xiang * can; // 參
} han; //函

typedef union {
    double shu;
    str ming;
    han han;
} zhi;  

typedef enum {
    ALLOCATION_MEMROY_FAILED
} error_code; //錯誤代碼

typedef enum {
    ADD_EXISTING_XIANG //試圖加入已存在的項至庫
} exception;

/* 印出錯誤，並結束程式 */
void error(error_code e);
void raise_exception(exception e);

exception last_exception;

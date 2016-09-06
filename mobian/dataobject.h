#include <wchar.h>

typedef wchar_t * str;

typedef enum {
    KONG, /* 空，表示未初始的項，僅用在庫，來表示未初始的位置 */
    YUAN,
    HAN 
} xin; //型

typedef struct {
    xin xin;    //型
    void* zhi;  //值
} xiang; //項

typedef struct {
    str wei;     // 謂
    int zhi;     // 枝
    xiang * can; // 參
} han; //函

#include <wchar.h>

typedef enum {
    YUAN,
    HAN 
} xin; //型

typedef struct {
    xin xin;    //型
    void* zhi;  //值
} xiang; //項

typedef struct {
    wchar_t * wei;  // 謂
    int zhi;       // 枝
    xiang * can;   // 參
} han; //函


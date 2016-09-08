#include <wchar.h>

typedef wchar_t * str;

typedef enum {
    KONG=0,//項結構初始時，型會設成0，因此0保留給空
    YUAN=1,
    HAN=2 
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

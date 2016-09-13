/* mobian.h
 *
 * 墨辨的基礎類型定義、錯誤處理
 * 
 * 
 * 墨辨中最基本的物件稱為項，有以下幾類：
 *
 * 元是無法再分解的項，實作上就是用一組字串表示，
 * 此字串稱為名。
 * 
 * 數是另一類無法再分解的物件，實作上以浮點數表示。
 *
 * 謂是一種複合物件，由其它項所組成，實作以名、枝及參數陣列來表示，
 * 枝指此謂的參數個數，
 * 而查詢上，以名及枝來找尋所有符合的參數集合。
 *
 *
 *
 * 使用者在查詢知識庫，會以名載入元或謂，
 *
 * 稜於花蓮美崙
 * 20160819 週五
 * 20160913 週二
 *
 */
#include <wchar.h>
#include <stdbool.h>

typedef wchar_t * str;

typedef enum {
    KONG=0,   //項結構初始時，型會設成0，因此0保留給空
    YUAN=1,
    WEI=2, 
    SHU=3
} xin; //型

/* 
 * 項的值其處理由型去決定
 * 目前分成 2 類，一是數字，二是指標指向資料儲存槽。
 */
typedef union {
    double shu;
    void * data;
} zhi;  

typedef struct {
    xin xin;    //型
    zhi zhi;  //值
} xiang; //項

typedef struct _canlian {
    xiang* canzu; /* 參組為一組項 */
    struct _canlian * next; /* 下一筆參組 */
} canlian_t;

/*
 * 謂可由名、枝及參串所定義，
 * 也就是輸入名、枝及參可知是否於定義中。
 * 
 * 謂可視為關聯資料庫中的表，名是表名而參則是紀錄。
 * 每一筆紀錄稱作實，可用來表示一件事實，以下就是一個實的例子：
 * 男(張簡稜剛)
 * 可解釋在男這個表，有一個元叫張簡稜剛
 */
typedef struct {
    str ming;    /* 名表示謂的名稱*/
    int zhi;     /* 枝表示參的數目 */
    canlian_t canlian; /* 參鏈，儲放紀錄，以鏈結串列實作，未來或許可以資料庫實作 */
} wei;

// 加入元的定義 
bool add_yuan(str ming); 

// 以名判斷元是否存在
bool has_yuan(str ming); 

// 加入實，擴增謂的定義
bool add_shi(str ming, int zhi, xiang* can); 

// 實是否存在，紀錄是否存在謂
bool has_shi(str ming, int zhi, xiang* can); 

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

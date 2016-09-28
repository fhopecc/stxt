/* mobian.h
 *
 * 墨辨的基礎類型定義、錯誤處理
 * 
 * 
 * 墨辨中最基本的物件稱為項，有以下幾類：
 *
*
 * 組是一種複合物件，由其它項所組成，
 * 實作以名、枝及參數陣列來表示，
 * 枝指此謂的參數個數，
 * 而查詢上，以名及枝來找尋所有符合的參數集合。
 *
 * 實作上，所有的複合結構都是指標，並均動態配置物件。
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

typedef enum { BIAN, SHU, FU, ZU} lei; //類為項的類別名
typedef enum { SHI, FA } xin; //型為句的分類

/* 項 */
typedef struct _xiang {
    lei lei;     //類
    void * data; // 實際儲存資料的地方
    struct _xiang * zhi;   // 值，變數綁定項目儲存處
} * xiang; //項

typedef xiang *xiangs; // 項的複數

/* 組 */
typedef struct _zu {
    str ming;   // 名
    size_t wei; // 維 
    xiangs can; // 參 
} * zu;

/* 2.1. */
typedef struct _ju {
    xin xin;    
    size_t chang;  //長，表示句子的項數，若為法，則第一個項是頭
    xiangs lie; //列，表示句子的項陣列。
} * ju;

/* 句鏈 */
typedef struct _jus { //因為詞的句集合，僅由定義的先後逐筆核對，故以鏈實作。
    ju ju;
    struct _jus *next;
} * jus;

/* 詞 */
typedef struct _ci { //2.3.2
    str ming;
    size_t wei;
    jus jus;
} * ci;

// 建立項
xiang newshu(double shu); 
xiang newfu(str fu); 
xiang newbian(str bian); 

// 存取屬性
double getshu(xiang x);
str getming(xiang x);

/* 變 */
/* 合一.2.1. */
bool isbangdin(xiang b); 
void bangdin(xiang b, xiang z); 
void unbangdin(xiang b);//去綁定 
xiang getzhi(xiang b);

/* 組 */
xiang newzu(str ming, size_t wei, xiangs can); 
size_t getwei(xiang x); //1.1.
xiangs getcan(xiang x); //1.

// 存取句屬性
double getchang(ju j);
xiangs getlie(ju j);

// 以名判斷元是否存在
bool has_yuan(str ming); 

/* 句 */
void newshi(xiang x); //2.1.
void newfa(xiang tou, size_t chang, xiangs ti); //2.2.

/* 庫 */
void add(ju j);

/* 詞 */
ci getci(str ming, size_t wei); //2.3.2

/* 合一 */
bool ismingeq(xiang x1, xiang x2);
bool unify_xiang(xiang x1, xiang x2);

typedef enum {
    ALLOCATION_MEMROY_FAILED
} error; //錯誤代碼

typedef enum {
    ADD_EXISTING_XIANG //試圖加入已存在的項至庫
} exception;

/* 印出錯誤，並結束程式 */
void reaise_error(error e);
void raise_exception(exception e);

exception last_exception;

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

typedef enum { SHU, FU, BIAN, ZU} lei; //類為項的類別名
typedef enum { SHI, FA } xin; //型為句的分類

/* 項的實際值用指標，彈性較大 */
typedef struct _xiang {
    lei lei;    //型
    void * zhi; //值
    double shu; //數值
} * xiang; //項

typedef struct _zu {
    str ming;    /* 名表示謂的名稱*/
    size_t zhi;     /* 枝表示參的數目 */
    xiang* can;  /* 參鏈，儲放紀錄，以鏈結串列實作，未來或許可以資料庫實作 */
} * zu;

typedef struct _ju {
    xin xin;    
    size_t chang;  //長，表示句子的項數，若為法，則第一個項是頭
    xiang* lie; //列，表示句子的項陣列。
} * ju;

// 建立項
xiang newshu(double shu); 
xiang newfu(str fu); 
xiang newbian(str bian); 
xiang newzu(str ming, size_t zhi, xiang* can); 

// 存取屬性
double getshu(xiang x);
str getming(xiang x);
xiang* getcan(xiang x);

// 建立句
ju newshi(size_t chang, xiang* lie);
ju newfa(size_t chang, xiang* lie);

// 存取句屬性
double getchang(ju j);
xiang* getlie(ju j);

// 以名判斷元是否存在
bool has_yuan(str ming); 

// 加入實，擴增謂的定義
bool add_shi(str ming, int zhi, xiang* can); 

// 實是否存在，紀錄是否存在謂
bool has_shi(str ming, int zhi, xiang* can); 

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

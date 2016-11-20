/* mobian.h
 *
 * 稜於花蓮美崙
 * 20160819 週五
 * 20160913 週二
 * 20161016 週日
 *
 */
#include <wchar.h>
#include <stdbool.h>

typedef enum {
    OK,
    MEMORY_OVERFLOW
} error;

typedef wchar_t * str;

typedef enum { BIAN, SHU, FU, ZU} lei; //類為項的類別名
typedef enum { SHI, 
               FA, 
               WEN //2.4.1.問句 
             } xin; //型為句的分類

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

/* 材 */
typedef struct _cai {
    size_t size;
    size_t capacity;
    ci *cis;
} * cai;

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

// 以名判斷元是否存在
bool has_yuan(str ming); 

/* 句 */
ju newshi(xiang tou); //2.1.
ju newfa(xiang tou, size_t chang, xiangs ti); //2.2.
ju newwen(size_t chang, xiangs zi); //2.4.1.問句

// 存取句屬性
size_t getchang(ju j);
xiang gettou(ju j);
xiang getzi(ju j, size_t i);
xiang getcheng(ju j);
xiangs getlie(ju j);
size_t juhash(ju j);

/* 詞 */
ci newci(str ming, size_t wei);
error addju2ci(ci c, ju j);
bool cihasju(ci c, ju j);
jus getjus(ci);

/* 材 */
cai newcai();
void addju(cai cai, ju j);
bool hasju(cai cai, ju j);
ci getci(cai cai, str ming, size_t wei);

/* 2.3.2.慮：從材取出符合特定項的詞 */
ci lu(cai c, xiang x);

/* 合一 */
bool ismingeq(xiang x1, xiang x2);
bool unify_xiang(xiang x1, xiang x2);
bool streq(const str s1, const str s2);

/* stable.h
 * Symbol table for mobian.
 * implemented using hashtable
 *
 * 20160819 週五
 * 
 * 語法：
 * 元
 * 變
 * 函
 */
typedef enum {
    ATOM,
    VAR,
    FUNC  
} type;

/*
 * var must be prefix with char '@'.
 */
typedef struct {
    w_char *name;
} var;

/*
 * atom 即為 arity 為 0 的 func
 * 張簡嘉品
 * 女人(張簡嘉品,175,69)
 */
typedef struct {
    w_char *name;
    int arity;
    func *pars[];
} func;

/*
 * 動物(@甲) :- 狗(@甲).
 */
typedef struct {
    fact *name;
    int arity;
    func *pars[];
} rule;

typedef struct {
    w_char *name; 
    type type;  
    w_char *definition; 
} record;

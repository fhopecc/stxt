/*
 * stable.h
 * Symbol table for mobian.
 * implemented using hashtable
 *
 * 20160819 週五
 *
 */
typedef enum {
    ATOM,
    VAR,
    FUNC  
} type;

typedef {
    w_char *name; 
    type type;  
    w_char *definition; 
} record;

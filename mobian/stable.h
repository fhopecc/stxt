/*
 * database.h
 * 
 * Analog to database in prolog. 
 *
 * 20160819 週五
 */

typedef enum {
    ATOM,
    FUNC, 
    VAR
} type;

typedef {
    w_char *name; 
    type type;  
    w_char *definition; 
} record;

typedef {
    record *
}

int get(str name);

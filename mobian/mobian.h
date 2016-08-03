typedef wchar_t *str;
typedef double num;

typedef struct _atom {
    str name;
} atom_t, *atom;

typedef struct _var{
    str name;
} var_t, *var;

var new_var(str name);

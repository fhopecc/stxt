#include <stdlib.h>
#include <stdio.h>
#include <wchar.h>
#include <locale.h>
#include "mobian.h"

int main(void){
    str w = L"墨辨\n";
    var v = new_var(L"變數");
    setlocale(LC_ALL, ".950");
    char *mbs = (char*)malloc(50);
    wcstombs(mbs, v->name, 50);
    printf(mbs);
    return 0;
}

var new_var(str name) {
    var v = (var)malloc(sizeof(var_t));
    v -> name=name;
    return v;
}


#include <stdlib.h>
#include <stdio.h>
#include <wchar.h>
#include <locale.h>
#include "mobian.h"

void print_error(str errmsg) {
    setlocale(LC_ALL, ".950");
    char *mbs = (char*)malloc(255);
    wcstombs(mbs, errmsg, 255);
    printf(mbs);
}

void error(error_code e) {
    switch(e) {
        case ALLOCATION_MEMROY_FAILED:
            print_error(L"發生錯誤：記憶體配置失敗");
            abort();
    }
}

void raise_exception(exception e) {
    last_exception=e;
}

#include <stdlib.h>
#include <stdio.h>
#include <wchar.h>
#include <locale.h>

int main(void){
    setlocale(LC_ALL, ".950");
    char *mbs = (char*)malloc(50);
    wcstombs(mbs, L"墨辨\n", 50);
    printf(mbs);
    return 0;
}

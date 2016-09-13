#include <stdio.h>
#include <assert.h>
#include "ku.h"

int main(void) {
    add_yuan(L"張簡稜剛");
    assert(has_yuan(L"張簡稜剛"));
    assert(has_yuan(L"沈懿嬅")==false);
    add_yuan(L"Robert");
    assert(has_yuan(L"Robert"));
}

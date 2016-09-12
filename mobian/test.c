#include <stdio.h>
#include <assert.h>
#include "ku.h"

int main(void) {
    /*
    // 員工(張簡稜剛,183,85,B,私立南台科技大學)
    han h1 = { 
        .wei=L"員工" 
        .zhi=5
        .can
    };
    */
    xiang h1;
    h1.wei = L"男人";
    h1.zhi = 1;
    h1.can 

    add_yuan(L"張簡稜剛");
    assert(has_yuan(L"張簡稜剛"));
    assert(has_yuan(L"沈懿嬅")==false);
    add_yuan(L"Robert");
    assert(has_yuan(L"Robert"));
    add_han(L"張簡稜剛"
}

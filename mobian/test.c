#include <wchar.h>
#include <stdlib.h>
#include <stdio.h>
#include <assert.h>
#include "mobian.h"

void test_object(void) {
    size_t h;
    xiang x = newshu(1979);
    assert(getshu(x)==1979);   
    x = newfu(L"張簡稜剛");
    assert(wcscmp(getming(x), L"張簡稜剛")==0);
    x = newbian(L"@甲");
    assert(wcscmp(getming(x), L"@甲")==0);
    x = newzu(L"職員", 3, (xiang[3]){newfu(L"張簡稜剛"), newfu(L"男"), newshu(1979)});
    assert(wcscmp(getming(x), L"職員")==0);
    assert(getzhi(x)==3);
    assert(getcan(x)[2]->lei==SHU);
    assert(wcscmp(L"男", getming(getcan(x)[1]))==0);
    ju j = newshi(3,(xiang[3]){newfu(L"張簡稜剛"), newfu(L"男"), newshu(1979)});
    assert(getchang(j)==3);
    assert(getming(getlie(j)[1])==L"男");
    assert(getlie(j)[1]->lei==FU);
    assert(getshu(getlie(j)[2])==1979);
}

void test_hash() {
    assert(xianghash(newshu(1979))==1979);
    assert(xianghash(newshu(181.3))==181);
    assert(xianghash(newshu(-1457))==-1457);
    assert(strhash(L"張簡稜剛")==-492411665);
    assert(xianghash(newfu(L"張簡稜剛"))==-492411665);
    assert(-488049750== 
            xianghash(newzu(L"職員", 3, (xiang[3]){ newfu(L"張簡稜剛")
                                                  , newfu(L"男")
                                                  , newshu(1979)})));
    assert(-492379676==
                 juhash(newshi(3, (xiang[3])
                                { newfu(L"張簡稜剛")
                                , newfu(L"男")
                                , newshu(1979)})));
}

int main(void) {
    test_object();
    test_hash();
}

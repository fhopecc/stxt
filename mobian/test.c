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
    /* 組 */
    x = newzu(L"職員", 3, (xiang[3]){newfu(L"張簡稜剛"), newfu(L"男"), newshu(1979)});
    /* 1 */
    assert(wcscmp(getming(x), L"職員")==0);
    assert(getwei(x)==3);
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

/* 合一 */
void test_unify_xiang() {
    xiang b, f, b1, b2, x1, x2;
    // 0.1.
    assert(unify_xiang(newshu(183), newshu(183)));
    assert(!unify_xiang(newshu(183), newshu(729)));
    // 1.
    assert(unify_xiang(newfu(L"張簡稜剛"), newfu(L"張簡稜剛")));
    assert(!unify_xiang(newfu(L"張簡稜剛"), newfu(L"張簡嘉品")));
    // 1.1
    assert(!unify_xiang(newfu(L"張簡稜剛"), newshu(729)));
    //2.1.
    b = newbian(L"@甲");f = newfu(L"張簡稜剛");
    assert(unify_xiang(b,f));
    assert(isbangdin(b));
    unbangdin(b);
    assert(!isbangdin(b));

    //2.1.1.
    assert(getzhi(newbian(L"@甲"))==NULL);
    assert(!isbangdin(newbian(L"@甲")));

    //2.2.
    //@甲=@乙 -> @乙=@甲
    b1 = newbian(L"@甲");
    b2 = newbian(L"@乙");
    assert(unify_xiang(b1,b2));
    assert(getzhi(b1) == b2);
    assert(getzhi(b2) == b1);
    //2.3.
    b1 = newbian(L"@甲");
    unify_xiang(b1, newfu(L"張簡稜剛")); 

    //@甲=張簡稜剛 == 張簡稜剛
    assert(unify_xiang(b1, newfu(L"張簡稜剛")));

    //@甲=張簡稜剛 != 張簡嘉品
    assert(!unify_xiang(b1, newfu(L"張簡嘉品")));

    //@甲=張簡稜剛 == @乙
    //@乙=張簡稜剛
    b2 = newbian(L"@乙");
    assert(unify_xiang(b1, b2));
    assert(wcscmp(getming(getzhi(b2)), L"張簡稜剛")==0);

    //2.4.
    //職員(張簡稜剛, 股長, 1979) ==  職員(@甲, 股長, @乙)
    x1 = newzu(L"職員", 3, (xiang[3]){newfu(L"張簡稜剛"), 
                                      newfu(L"股長"), 
                                      newshu(1979)});
    b1 = newbian(L"@甲");
    b2 = newbian(L"@乙");
    x2 = newzu(L"職員", 3, (xiang[3]){b1, newfu(L"股長"), b2});
    assert(unify_xiang(x1,x2));
    assert(wcscmp(getming(getzhi(b1)),L"張簡稜剛")==0);
    assert(getshu(getzhi(b2)) == 1979);

    //職員(張簡稜剛, 股長, 1979) !=  職員(@甲, 科長, @乙)
    b1 = newbian(L"@甲");
    b2 = newbian(L"@乙");
    x2 = newzu(L"職員", 3, (xiang[3]){b1, newfu(L"科長"), b2});
    assert(!unify_xiang(x1,x2));

}

int main(void) {
    test_object();
    test_hash();
    test_unify_xiang();
}

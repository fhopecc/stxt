#include <wchar.h>
#include <stdlib.h>
#include <stdio.h>
#include <assert.h>
#include "mobian.h"

void test_object();
void test_ju();
void test_ci();
void test_cai();
void test_unify_xiang();

int main() {
    test_object();
    test_unify_xiang();
    test_ju();
    test_ci();
    test_cai();
}

void test_object() {
    size_t h;
    xiang x = newshu(1979);
    assert(getshu(x)==1979);   
    x = newfu(L"張簡稜剛");
    assert(streq(getming(x), L"張簡稜剛"));
    x = newbian(L"@甲");
    assert(wcscmp(getming(x), L"@甲")==0);
    /* 組 */
    x = newzu(L"職員", 3, (xiang[3]){newfu(L"張簡稜剛"), newfu(L"男"), newshu(1979)});
    /* 1 */
    assert(wcscmp(getming(x), L"職員")==0);
    assert(getwei(x)==3);
    assert(getcan(x)[2]->lei==SHU);
    assert(wcscmp(L"男", getming(getcan(x)[1]))==0);
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

void test_ju() {
    //2.1.1.
    assert(wcscmp(getming(gettou(newshi(newfu(L"花紅")))),L"花紅")==0);
    //2.2.1.
    //祖孫(@祖, @孫) :- 父子(@祖, @父), 父子(@父, @孫).
    ju j =newfa(newzu(L"祖孫", 2, (xiang[2]){newbian(L"@祖"), newbian(L"@孫")}), 2
         ,(xiang[2]){newzu(L"父子", 2, (xiang[2]){newbian(L"@祖"), newbian(L"@父")})
         ,newzu(L"父子", 2, (xiang[2]){newbian(L"@父"), newbian(L"@孫")})
         });
    assert(getchang(j)==2);
    assert(streq(getming(getzi(j, 0)),L"父子"));
}

void test_ci() {
    ci c = newci(L"花紅", 0);
    addju2ci(c, newshi(newfu(L"花紅")));
    addju2ci(c, newshi(newfu(L"柳綠")));
    assert(cihasju(c, newshi(newfu(L"花紅"))));
    assert(!cihasju(c, newshi(newfu(L"花綠"))));
    assert(cihasju(c, newshi(newfu(L"柳綠"))));
}

void test_cai() {
    cai c = newcai();
    addju(c, newshi(newzu(L"父子", 2, (xiang[2]){newfu(L"金水"), newfu(L"稜剛")})));
    addju(c, newshi(newzu(L"父子", 2, (xiang[2]){newfu(L"稜剛"), newfu(L"弼叡")})));
    addju(c, newfa(newzu(L"祖孫", 2, (xiang[2]){newbian(L"@祖"), newbian(L"@孫")}), 2
         ,(xiang[2]){newzu(L"父子", 2, (xiang[2]){newbian(L"@祖"), newbian(L"@父")})
         ,newzu(L"父子", 2, (xiang[2]){newbian(L"@父"), newbian(L"@孫")})
         }));
    assert(hasju(c, newshi(newzu(L"父子", 2, (xiang[2]){newfu(L"稜剛"), newfu(L"弼叡")}))));
    assert(!hasju(c, newshi(newzu(L"父女", 2, (xiang[2]){newfu(L"稜剛"), newfu(L"弼叡")}))));
}

normalLWH(L, W, H, NL, NW, NH) :-
    
%最大長寬高及重量
maxLWHWt(45, 30, 30, 5000, jiaoHuoBian). % 交貨便 
maxLWHWt(27.5, 20, 6, 5000, jinJiDai). 
maxLWHWt(21.8, 12, 12, 5000, jinJiDai).
maxLWHWt(23, 14, 13, 5000, xiaoZiHe).   %全家小資盒
maxLWHWt(25, 20, 5, 5000, xiaoZiHe).

%塞得下，就便宜
%總長小於 90，邊小於 45, 重量小於5kg
maxTlSlWt(90, 45, 5000, dianDaoDian).  % 全家店到店 
maxTlSlWt(90, 45, 5000, ezShip).       % OK, 萊爾富 ezShip 
maxTlSlWt(90, 45, 1000, xiaoBaoGuaHao).%郵局小包掛號

%長小於 16寬小於 28, 重量小於1kg
maxLWWt(28, 16, 1000, bianLiBao1). % 郵局便利包
maxLWWt(32, 23, 1000, bianLiBao2).
maxLWWt(38, 28, 1000, bianLiBao3).
maxLWWt(33.7, 26, 5000, jinJiDai). % 黑貓經濟袋
maxLWWt(40, 28, 5000, xiaoZiDai).  %  全家小資袋

matchLWHWt(L, W, H, Wt, S) :-
    maxLWHWt(ML, MW, MH, MWt, S), L =< ML , W =< MW, H =< MH , Wt =< MWt.

matchLWHWt(L, W, H, Wt, S) :-
    maxTlSlWt(MTl, Sl, MWt, S), 
    Tl is L + W + H, 
    Tl =< MTl, 
    L =< Sl, W =< Sl, H =< Sl, 
    Wt =< MWt.

matchLWWt(L, W, Wt, S) :-
  maxLWWt(ML, MW, MWt, S), 
  L =< ML, W =< MW, Wt =< MWt.

fee(xiaoZiHe, 65).
fee(bianLiBao1, 37).
fee(bianLiBao2, 50).
fee(bianLiBao3, 62).
fee(jinJiDai, 88).
fee(xiaoZiDai, 85).
fee(jiaoHuoBian, 60).
fee(dianDaoDian, 60).
fee(ezShip, 60).

% 20 元是掛號資費，每100g是10元
feeXiaoBaoGuaHao(Wt, Fee) :- 
    Fee is ceiling(Wt / 100) * 10 + 20.

% 印刷品掛號
feeYingShuaPing(Wt, Fee) :- 
    Wt =< 50, Fee is 3.5. 
feeYingShuaPing(Wt, Fee) :- 
    Wt >= 51, Wt =< 100, 
    Fee is 7. 
feeYingShuaPing(Wt, Fee) :- 
    Wt >= 101, Wt =< 250, 
    Fee is 10. 
feeYingShuaPing(Wt, Fee) :- 
    Wt >= 251, Wt =< 500, 
    Fee is 20. 
feeYingShuaPing(Wt, Fee) :- 
    Wt >= 501, Wt =< 1000, 
    Fee is 35. 
feeYingShuaPing(Wt, Fee) :- 
    Wt >= 1001, Wt =< 2000, 
    Fee is 55. 
feeYingShuaPingGuaHao(Wt, Fee):- 
    feeYingShuaPing(Wt, F),
    Fee is F + 20.

getFee(L, W, H, Wt, S, Fee) :- 
    matchLWHWt(L, W, H, Wt, S),
    fee(S, Fee). 
getFee(L, W, _, Wt, S, Fee) :- 
    matchLWWt(L, W, Wt, S),
    fee(S, Fee). 
getFee(L, W, H, Wt, xiaoBaoGuaHao, Fee) :- 
    matchLWHWt(L, W, H, Wt, xiaoBaoGuaHao),
    feeXiaoBaoGuaHao(Wt, Fee). 
getFee(_, _, _, Wt, yingShuaPingGuaHao, Fee) :-
    feeYingShuaPingGuaHao(Wt, Fee). 

haveCheaper(L, W, H, Wt, S1, F1):-
    getFee(L, W, H, Wt, S1, F1), 
    getFee(L, W, H, Wt, S2, F2), 
    S1\=S2, F1 >= F2.

getCheapest(L, W, H, Wt, S1, F1):-
    getFee(L, W, H, Wt, S1, F1), 
    not(haveCheaper(L, W, H, Wt, S1, F1)), 
    !.

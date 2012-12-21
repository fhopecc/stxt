import Test.HUnit
import Data.Either
import Mobian
import Parser

main = runTestTT testUnify

-- test code
testData = [ 
    Func "a" [Var "x"] :- [Func "b" [Var "x"]],
    Func "b" [Var "x"] :- [Func "e" [Var "x"]],
    Func "l" [Atom "karl", Var "x"] :- [Func "a" [Var "x"]],
    Fact (Func "b" [Atom "franz"]), 
    Fact (Func "e" [Atom "hansi"])
    ] 

testData2 = [ 
    Func "a" [Var "x"] :- [Func "b" [Var "x"]],
    Func "b" [Var "x"] :- [Func "e" [Var "x"]],
    Func "l" [Atom "karl", Var "x"] :- [Func "a" [Var "x"]],
    Fact (Func "b" [Atom "franz"]), 
    Fact (Func "e" [Atom "hansi"])
    ] 

testShowTerm = test [
   "testShowAtom" ~: "Atom a -> a"  ~: 
   "a" ~=? (show $ Atom "a"),

   "testShowFunc" ~: "Should be f(a b)"  ~: 
   "f(a b)" ~=? (show $ Func "f" [Atom "a", Atom "b"])
   ]

testUnify = test [
    "testUnify" ~: "testUnify" ~: [
        Var "x" := Atom "x", 
        Var "f" := Func "f" [Atom "x", Atom "o"],
        Var "q" := Atom "q", 
        Var "s" := Atom "s" 
    ] ~=? unify [
        Atom "x" := Var "x", 
        Var "f"  := Func "f" [Var"x", Atom "o"],
        Func "g" [Atom "q", Var "s"] := 
        Func "g" [Var"q", Atom "s"]
    ],
    "testUnify2" ~: "testUnify2" ~: [
        Var "x" := Atom "hansi" 
    ] ~=? unify [
        Func "l" [Atom "karl", Atom "hansi"] := 
        Func "l" [Atom "karl", Var "x"] 
    ],
    "testUnify3" ~: "testUnify3" ~: [
        Var "x" := Atom "hansi" 
    ] ~=? unify [
        Func "a" [Atom "hansi"] := Func "a" [Var "x"] 
    ],
    "testUnify4" ~: "testUnify4" ~: 
        [] 
    ~=? unify [
        Func "e" [Atom "hansi"] := Func "e" [Atom "hansi"] 
    ]
    ]

testResolve = test [
    "test_sub" ~: "test_sub" ~: 
        Atom "hansi"
     ~=? 
       sub (Var "x") [Var "x" := Atom "hansi"],
    "test_sub" ~: "test_sub" ~: 
        Func "l" [Atom "hansi", Atom "eagle", Atom "c"]
     ~=? 
       sub (Func "l" [Var "x", Var "y", Atom "c"])
                  [Var "x" := Atom "hansi", Var "y" := Atom "eagle"],
    "testResolve" ~: "testResolve" ~: 
        (Goal [Func "a" [Atom "hansi"]], [Var "x" := Atom "hansi"])
     ~=? 
       resolve (Goal [Func "l" [Atom "karl", Atom "hansi"]]) 0
               (Func "l" [Atom "karl", Var "x"] :- [Func "a" [Var "x"]])
                ,
    "test_resolve2" ~: "test_resolve2" ~: 
        (Goal [Func "b" [Atom "hansi"]], [Var "x" := Atom "hansi"])
     ~=? 
       resolve (Goal [Func "a" [Atom "hansi"]]) 0
                (Func "a" [Var "x"] :- [Func "b" [Var "x"]])
                ,
    "testResolve3" ~: "testResolve3" ~: 
        (Goal [Func "e" [Atom "hansi"]], [Var "x" := Atom "hansi"])
     ~=? 
       resolve (Goal [Func "b" [Atom "hansi"]]) 0
               (Func "b" [Var "x"] :- [Func "e" [Var "x"]]),
    "testResolve4" ~: "testResolve4" ~: 
        (NullGoal, [])
     ~=? 
       resolve (Goal [Func "e" [Atom "hansi"]]) 0 
               (Fact (Func "e" [Atom "hansi"]))
               ,
    "testResolve5'" ~: "test resolve for rename" ~: 
        (Goal [Func "q" [Var "x1", Func "f" [Func "f" [Var "x1"]]]], 
         [Var "x" := Func "f" [Var "x1"],
          Var "y" := Func "f" [Func "f" [Var "x1"]]])
     ~=? 
       resolve (Goal [Func "p" [Var "x", Func "f" [Var "x"]]]) 0
                (Func "p" [Func "f" [Var "x"], Var "y"] :- 
                 [Func "q" [Var "x", Var "y"]]), 
    "testRefute" ~: "testRefute" ~: 
        (NullGoal, [Var "x" := Atom "hansi"])
     ~=? 
        let (fgoal, subs) = refute 
               (Goal [Func "l" [Atom "karl", Atom "hansi"]]) testData 
        in (fgoal, computeAnswer(subs))
        ,
    "testUnify" ~: "testUnify" ~: [
        Var "y" := Var "x" 
    ] ~=? unify [
        Func "l" [Atom "karl", Var "y"] := 
        Func "l" [Atom "karl", Var "x"] 
    ],
    "testResolve6" ~: "test resolve 6" ~:
       (Goal [Func "a" [Var "x"]], [Var "y" := Var "x"])
     ~=? 
        resolve (Goal [Func "l" [Atom "karl", Var "y"]]) 0
                (Func "l" [Atom "karl", Var "x"] :- 
                [Func "a" [Var "x"]]), 
    "testResolve7" ~: "test resolve 7" ~:
       (Goal [Func "e" [Var "x1"]], [Var "x" := Var "x1"])
     ~=? 
        resolve (Goal [Func "a" [Var "x"]]) 0
                (Func "a" [Var "x"] :- 
                [Func "e" [Var "x"]]), 
    "testResolve8" ~: "test resolve 8" ~:
       (NullGoal, [Var "x1" := Atom "hansi"])
     ~=? 
        resolve (Goal [Func "e" [Var "x1"]]) 0
                (Fact (Func "e" [Atom "hansi"])), 
    "testComputeAnswer" ~: "test compute answer" ~:
       [Var "y" := Atom "hansi"]
     ~=? 
       (computeAnswer (snd (refute (Goal [Func "l" [Atom "karl", Var "y"]])
                       testData))),
    "testRefute2" ~: "testRefute2" ~: 
       (NullGoal, [Var "y" := Atom "hansi"])
     ~=? 
       let (fgoal, subs) = refute (Goal [Func "l" [Atom "karl", Var "y"]])
            testData
       in (fgoal, computeAnswer(subs))
    ]
--
--Func "e" [Var "y"]
-- test sub
testSubData1 = [
    Var "x" := Atom "hansi",
    Var "y" := Atom "eagle"
    ] 

testSubData2= [
    Var "z" := Var "x"
    ]

testSubstitute = test [
    "testToSubstitution" ~: "testToSubstitution" ~: 
        Atom "hansi"
     ~=? 
        toSub testSubData1 (Var "x"),
    "testComposition" ~: "testComposition" ~: 
        Atom "hansi"
     ~=?
        ((toSub testSubData1) . (toSub testSubData2) $ Var "z")
    ]

testVars = test [
    "testVarsFunc" ~: "test variables in function" ~:
        [Var "v1", Var "v2"]
    ~=?
        (vars $ Func "f" [Var "v1", Var "v2", Atom "a"]), 
    "testVarsVar" ~: "test variables in variable" ~:
        [Var "v1"]
    ~=?
        (vars $ Var "v1"), 
    "testVarsAtom" ~: "test variables in Atom" ~:
        []
    ~=?
        (vars $ Atom "a")
    ]

testRename = test [
    "testRename" ~: "test rename" ~:
        (Func "p" [Func "f" [Var "x1"], Var "y"] :-
         [Func "q" [Var "x1", Var "y"]])
    ~=?
        (rename "1" (Func "p" [Var "x", Func "f" [Var "x"]])
            (Func "p" [Func "f" [Var "x"], Var "y"] :-
             [Func "q" [Var "x", Var "y"]]))
    ]

testGoal = "死($人)."

testProgram = "死($人) :- 人($人).\n\
              \人(亞里士多德)."

testGoal2 = "祖孫($祖,張簡稜剛)."
testProgram2 = "父子(張簡金水,張簡稜剛).\n\
               \父子(張簡蔭,張簡金水).\n\ 
               \祖孫($祖,$孫) :- 父子($祖,$父),父子($父,$孫).\n"


testParser = test [
    "testParser" ~: "parse atom" ~:
        [Fact $ Atom "張簡稜剛"]
    ~=?
        (case parseDB "張簡稜剛." of
            Left  e  -> fail (show e)
            Right r  -> r), 
    "testParser2" ~: "parse var" ~:
        [Fact $ Var "父"]
    ~=?
        (case parseDB "$父." of
            Left  e  -> fail (show e)
            Right r  -> r),
    "testParser3" ~: "test parse func" ~:
        [Fact $ Func "父親" [Atom "張簡金水", Atom "張簡稜剛"]]
    ~=?
        (case parseDB "父親(張簡金水,張簡稜剛)." of
            Left  e  -> fail (show e)
            Right r  -> r),
    "testParser4" ~: "test parse func 2" ~:
        [Fact $ Func "父親" [Func "男性" [Atom "張簡金水"], Atom "張簡稜剛"]]
    ~=?
        (case parseDB "父親(男性(張簡金水),張簡稜剛)." of
            Left  e  -> fail (show e)
            Right r  -> r),
    "testParser5" ~: "test parse func 3" ~:
        [Fact $ Func "父親" [Var "祖父",Func "父親" [Var "父", Var "子"]]]
    ~=?
        (case parseDB "父親($祖父,父親($父,$子))." of
            Left  e  -> fail (show e)
            Right r  -> r),
    "testParser6" ~: "test parse rules" ~:
        [Fact $ Atom "張簡金水",Fact $ Atom "張簡稜剛"]
    ~=?
        (case parseDB "張簡金水.\n張簡稜剛." of
            Left  e  -> fail (show e)
            Right r  -> r), 
    "testParser7" ~: "test parse rule" ~:
        [Atom "祖父" :- [Func "父親" [Var "祖父",Func "父親" [Var "父", Var "子"]]]]
    ~=?
        (case parseDB "祖父 :- 父親($祖父,父親($父,$子))." of
            Left  e  -> fail (show e)
            Right r  -> r), 
    "testParser8" ~: "test parse rule 2" ~:
        [Func "祖父" [Var "祖父", Var "子"] :- [Func "父親" [Var "祖父",Func "父親" [Var "父", Var "子"]]]]
    ~=?
        (case parseDB "祖父($祖父,$子) :- 父親($祖父,父親($父,$子))." of
            Left  e  -> fail (show e)
            Right r  -> r), 
    "testParser9" ~: "test parse rule 2" ~:
        [(Func "死" [Var "人"] :- [Func "人" [Var "人"]]), (Fact $ Func "人" [Atom "亞里士多德"])]
    ~=?
        (case parseDB "死($人) :- 人($人).\n\
                      \人(亞里士多德)." of
            Left  e  -> fail (show e)
            Right r  -> r), 
    "testParssGoal" ~: "test parse goal" ~:
        Goal [Func "死" [Var "人"]]
    ~=?
        (case parseGoal "死($人)." of
            Left  e  -> NullGoal
            Right r  -> r), 
    "testParss" ~: "test parse goal2" ~:
        [Var "人" := Atom "亞里士多德"]
    ~=?
        (case parseGoal testGoal2  of
            Left  e  -> []
            Right r  -> case parseDB testProgram2 of
                        Left  e  -> []
                        Right db  -> computeAnswer(snd (refute r db))
            )
    ]

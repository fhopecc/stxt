import Test.HUnit

data Term = Atom String
           |Var  String
           |Func String [Term]
            deriving Eq
-- EX. q :- p
data Rule = 
    (:-) Term [Term] |
    Rule Term
    deriving Eq

data Goal = 
    Goal [Term] 
    deriving Show

instance Show Rule where show = showRule

instance Show Term where show = showTerm

data Equ = (:=) Term Term
           deriving Eq

instance Show Equ where show = showEqu


main = runTestTT testUnify
    --print (Func "r" [Atom "a1", (Func "r2" [Var "x"])])
    --print $ varsub' (Equ (Var "x") (Atom "a")) (Var "x") 
    --print $ varsub' (Equ (Var "x") (Atom "a")) (Func "g" [Atom "a1", Var "x"])
    --print $ varsub' (Equ (Var "x") (Atom "a")) (Func "w" [Var "s", Atom "a"]) 
    --print $ varsub' (Equ (Var "x") (Atom "a")) (Var "s")
    --print $ varsub' (Equ (Var "x") (Atom "a")) (Atom "a")
    --print es
    --print "--"
    --print $ unify 9 $ es
    --
    --dounify [Equ (Atom "x") (Var "x"), 
    --         Equ (Var "f") (Func "f" [Var"x", Atom "o"]),
    --         Equ (Func "g" [Atom "q", Var "s"]) 
    --         (Func "g" [Var"q", Atom "s"])
    --         ]
    --print $ (Atom "x") :- [(Func "f" [Var"x", Atom "o"])]
    --print $ resolve (Goal [Atom "q"]) [(Atom "q") :- [Atom "p"], Rule (Atom "p")] 0

ruleHead::Rule -> Term
ruleHead (head :- _) = head
ruleHead (Rule head) = head

ruleTail::Rule -> [Term]
ruleTail (_ :- tail) = tail
ruleTail (Rule head) = []
 
resolve:: Goal -> [Rule] -> Int -> Goal
resolve (Goal ls) rules index = 
    Goal $ subGoal index (ruleTail r) ls
    where 
    l = ls !! index
    r = head $ filter (\r -> ruleHead r == l) rules
    subGoal i s ls = take i ls ++ s ++ drop (i+1) ls

-- i index oes old equs es equs
dounify :: [Equ] -> [Equ]

dounify es = 
    if nes == es
    then es
    else dounify nes
    where nes = iterate_unify 0 es

iterate_unify :: Int -> [Equ] -> [Equ]

iterate_unify i es = 
    if i < length es 
    then iterate_unify (i+1) (unify i es)
    else es

unify :: Int -> [Equ] -> [Equ]

-- i for selected equation index
-- es for equations
unify i es = case (!!) es i of 
    --1.下面形式則表示無法一致化
    --1.1. f = g 其中f及g 是相異原子。
    e@(Atom a := Atom b)  -> 
        if a/=b 
        then error $ "fail for diff atom:\n" ++ (show e)
        --3.a=a 其中a均為元式，則刪除此等式
        else delEqu i es
    --1.2. f = g 其中f為複合子及g是原子，f為原子及g是複合子。
    e@(Func _ _ := Atom _) -> 
        error $ "fail for func = atom:\n" ++ (show e)
    --1.3. f(...) = g(...)  其中f及g 是相異函子。
    e@(Func a as := Func b bs) -> 
        if a /= b  
        then error $ "fail for diff func:\n" ++ (show e)
        --1.4. f(a1, a2, ..., aN) = f(b1, b2, ..., bM) 其中N及M數目不同。
        else if length as /= length bs
            then error $ "fail for diff func args:\n"++ (show e)
            --4. f(a1, a2, ..., aN) = f(b1, b2, ..., bN) 則用 
            --   ai = bi 等式組取代
            else subequ i (zipWith (\ a b -> a := b) as bs) es
    --2.$X=$X 其中$X均為變數，則刪除此等式
    Var a := Var b  -> delEqu i es
    --5.t = X 其中 X 為變數而 t為非變數，則以 X = t取代
    t := (Var x)        -> subequ i [Var x := t] es
    e@(Var x := Func w as)  -> 
        --7.X = t 其中 X 為變數而 t為非變數，且 X 在 t 中，
        --  則因正向變數檢查而無法一致化。
        if Var x `elem` as
        then error $ "fail for positive var check:\n" ++ (show e)
        --6.X = t 其中 X 為變數而 t為非變數，且 X 未在 t 中，
        --  而存在其它等式中，後把其它等式出現的X 以 t 取代。
        else 
        let equ = (Var x := Func w as)
        in take i es `varsub` equ ++ [equ] ++ drop (i+1) es `varsub` equ
    Var x := t  -> 
        let equ = Var x := t
        in take i es `varsub` equ ++ [equ] ++ drop (i+1) es `varsub` equ
    --8.若無其它步驟可套用，則一致化成功。
  where
  -- p for posi, e for subs, es for equs
  subequ i e es = take i es ++ e ++ drop (i+1) es
  delEqu i es = take i es ++ drop (i+1) es

  
varsub :: [Equ] -> Equ -> [Equ]

varsub oes@(l := r:es) e  = 
  if (l := r) /= e
  then ( (varsub' l e := varsub' r e)):varsub es e
  else oes

varsub [] e = []

-- var substitute for the right side in the equation
varsub' ::Term -> Equ -> Term

varsub' (Var x) (v := t)  = if (Var x) == v then t else (Var x)
  
varsub' (Func w as) (v := t) = Func w (map (`varsub'` (v := t)) as)

varsub' t _ = t

showTerm::Term -> String

showTerm (Atom name) = name

showTerm (Var name)  = "$" ++ name

showTerm (Func name args) = name ++ 
    "(" ++ unwords (map showTerm args) ++ ")"

showEqu (t := u) = showTerm(t) ++ "=" ++ showTerm(u)

showRule (head :- body) = 
    showTerm(head) ++ " :- " ++ 
    foldr1 (\a b -> a ++ "," ++ b) (map show body) ++ "."

-- test code
testShowTerm = test [
   "testShowAtom" ~: "Atom a -> a"  ~: 
   "a" ~=? (show $ Atom "a"),

   "testShowFunc" ~: "Should be f(a b)"  ~: 
   "f(a b)" ~=? (show $ Func "f" [Atom "a", Atom "b"])
   ]

testUnify = 
    test [
       "testUnify" ~: "testUnify" ~:
        [(Var "x" := Atom "x"), 
         (Var "f" := Func "f" [Atom "x", Atom "o"]),
         (Var "q" := Atom "q"), 
         (Var "s" := Atom "s")
        ] ~=?
        (dounify [Atom "x" := Var "x", 
                   Var "f"  := Func "f" [Var"x", Atom "o"],
                   Func "g" [Atom "q", Var "s"] := 
                   Func "g" [Var"q", Atom "s"]
        ])
    ]

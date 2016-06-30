{-# LANGUAGE FlexibleContexts #-}
module Mobian(Term(Atom, Var, Func), MobianError(MobianError),
       Rule((:-), Fact), Goal(Goal,NullGoal),
       Equ((:=)),
       toSub, vars, rename, computeAnswer, 
       refute,  resolve, unify, u, sub) 
where
import Data.Maybe
import Data.List
import Debug.Trace
import System.IO
import System.Exit
--import GHC.IO.Encoding.CP950
import Control.Monad.Error
import Data.Either
type Label = String

type Unifier = [Equ]

-- Term for prolog Term
data Term = Atom Name
           |Var Name
           |Func Name [Term]
            deriving Eq

type Name = String 

instance Show Term where show = showTerm

-- get variables occurs Term
vars :: Term -> [Term]
vars (Func _ ts) = ts >>= vars -- list monad bind: concat (map vars ts) 
vars var@(Var _) = [var]
vars _ = []

-- Rule represents Rule
-- q :- p
-- Fact is q :- [] 
data Rule = 
     (:-) Term [Term] 
    |Fact Term
    deriving Eq

rHead::Rule -> Term
rHead (head :- _) = head
rHead (Fact head) = head

rTail::Rule -> [Term]
rTail (_ :- tail) = tail
rTail (Fact head) = []

subRule::Rule -> [Equ] -> Rule

subRule (t :- ts) equs =
    let subR = (`sub` equs)
    in 
        subR t :- map subR ts

subRule r _ = r

varsRule::Rule -> [Term]

varsRule r = 
    vars (rHead r) ++ (rTail r) >>= vars

instance Show Rule where show = showRule

-- Goal represents prolog goal
data Goal = Goal [Term] | NullGoal
            deriving (Show, Eq)

-- Equ represents equation
data Equ = (:=) {
        left  :: Term,
        right :: Term
    } deriving Eq

-- Env represents environment
data Env = Env {
        
    }

instance Show Equ where show = showEqu

compose :: [Equ] -> [Equ] -> [Equ]
compose sub1 sub2 = map equSub sub1
    where 
        equSub :: Equ -> Equ
        equSub e = (left e `sub` sub2) := (right e `sub` sub2)

computeAnswer :: [[Equ]] -> [Equ]

computeAnswer = foldr1 compose

--rename common variables in Rule
--  to make goal and rule without common variables
--  args:
--  pf for postfix
--  g  for goal
--  r  for rule
--  return rename rule
rename::String -> Term -> Rule -> Rule
rename pf g r  = 
    let gvars = vars g -- vars occurs in goal
        rvars = varsRule r -- vars occurs in rule
        cvars = gvars `intersect` rvars -- vars in common
        nvars = map pfvar cvars 
        subs  = zipWith (:=) cvars nvars 
    in 
        r `subRule` subs
    where pfvar (Var name) = Var (name ++ pf)

--refute::Goal -> [Rule] -> Goal
-- args:
--   g  for goal
--   i  for selected literal index
--   rs for rules
--   return (goal, unifers)

refute::Goal -> [Rule] -> (Goal, [[Equ]])
refute goal rules = refute' goal 0 rules 0 []

refute'::Goal -> Int -> [Rule] -> Int -> [[Equ]] -> (Goal, [[Equ]])

refute' goal@(Goal gs) gi rs ri unifiers = 
    let sg = gs !! gi -- sg for selected subgoal
        sr = rs !! ri -- sr for selected rule
        renameRule = rename $ show gi
        isRuleMatched r =
            let ruleHead = rHead (renameRule sg r)
            in  sg == ruleHead || (unify [sg := ruleHead]) /= []
        mr = find isRuleMatched rs -- mr for the first matchedRule 
        (nextGoal, nextUnifier) = 
            if isJust mr
            then resolve goal gi (fromJust mr)
            else error $ "fail to refute.\n" ++ "subgoal: " ++ show(sg) ++
                         "\nrule " ++ show(rs)
    in 
        if isRuleMatched sr
        then
            let 
                (nextGoal, nextUnifier) = resolve goal gi sr
            in 
                if nextGoal == NullGoal -- refute successful
                then (NullGoal, unifiers ++ [nextUnifier])  
                else refute' nextGoal 0 rs 0 (unifiers ++ [nextUnifier])
        else 
            if ri < (length rs - 1) then refute' goal gi rs (ri + 1) unifiers
            else 
                if gi < (length gs - 1) then refute' goal (gi + 1) rs 0 unifiers
                else error $ "fail to refute.\n" ++ "subgoal: " 
                             ++ show(sg) ++ "\nrule " ++ show(rs)
    
-- resolve 
-- args:
--   g  for goal
--   ls for literals in a goal
--   i  for selected literal index
--   rs for rules
--   return (goal, unifer)
resolve::Goal -> Int -> Rule -> (Goal, [Equ])

resolve (Goal ls) i r = 
    let sl = ls !! i -- sl for selected literal
        renameRule = rename "1"
        ar = renameRule sl r -- ar for apply rule
        unifier = unify [sl := rHead ar]
        rg = replaceLiteralWith (rTail ar) i ls -- replaced goal
        resolvent = Goal $ map (`sub` unifier) rg
    in 
        if resolvent /= Goal []
        then (resolvent, unifier)
        else (NullGoal, unifier)
    where 
        -- r for replacement
        -- i for index of selected literals
        -- ls for literals
        replaceLiteralWith r i ls = take i ls ++ r ++ drop (i+1) ls
    
-- apply substitution in input term
sub::Term -> [Equ] -> Term

sub var@(Var name) equs = 
    if apply_equ == Nothing 
    then var
    else right (fromJust apply_equ)
    where
    apply_equ = find (\e -> left e == var) equs

sub (Func name args) equs = 
    Func name (map (`sub` equs) args) 

sub term equs = term
    
-- unify
--
unify :: [Equ] -> Unifier

unify es = 
    if nes == es
    then es
    else unify nes
    where nes = unify' 0 es

unify' :: Int -> [Equ] -> [Equ]

unify' i es = 
    if i < length es 
    then unify' (i+1) (unify'' i es)
    else es

data MobianError = MobianError String deriving Show

instance Error MobianError where
    noMsg    = MobianError "Mobian has an error!"
    strMsg s = MobianError s

type Unification = Either MobianError

-- u for unifiy an equation
u :: Equ -> Unification [Equ]
u e@(Atom a := Atom b) = 
    if a == b then return [] -- a=a 其中a均為元式，則刪除此等式
    else throwError $ MobianError $ 
         "等式「"++show e ++ "」，因其中"++a++"及"++b++"為相異原子，\
         \一致化失敗!" 

u e@(f@(Func _ _) := a@(Atom _)) =
    throwError $ MobianError $ 
         "等式「"++show e ++ "」，因其中「"++show f++"」為複合子而「"++
         show a ++ "」為原子，一致化失敗!" 

u e@(f1@(Func a as) := f2@(Func b bs)) =
    if a /= b then throwError $ MobianError $ 
         "等式「"++show e ++ "」，因其中「"++show f1++"」及「"++
         show f2++"」為相異函，一致化失敗!" 
    else if length as /= length bs then throwError $ MobianError $ 
         "等式「"++show e ++ "」，因其中「"++show f1++"」引數長為"++
         show (length as)++"而「"++show f2++"」引數長為"++
         show (length bs)++"，一致化失敗!" 
            -- f(a1, a2, ..., aN) = f(b1, b2, ..., bN) 則用 
            -- ai = bi 等式組取代
            else return $ zipWith (\ a b -> a := b) as bs

-- $X=$X 其中$X均為變數，則刪除此等式
u e@(Var a := Var b) = if a == b then return [] else return [e]

-- t = X 其中 X 為變數而 t為非變數，則以 X = t取代
u (t := Var x) = return [Var x := t]

u e@(Var x := Func w as) = 
    if Var x `elem` as then throwError $ MobianError $ 
         "等式「"++show e ++ "」一致化失敗，因變數在函數之引數中出現\
         \(正向變數檢查)。"
        --6.X = t 其中 X 為變數而 t為非變數，且 X 未在 t 中，
        --  而存在其它等式中，後把其它等式出現的X 以 t 取代。
        else 
        let equ = (Var x := Func w as)
        in take i es `varsub` equ ++ [equ] ++ drop (i+1) es `varsub` equ

unify'' :: Int -> [Equ] -> [Equ]

-- i : selected equation index
-- es: equations
unify'' i es = case (!!) es i of 
    --1.下面形式則表示無法一致化
    e@(Atom a := Atom b)  -> 
        if a/=b 
        then []--error $ "fail for diff atom:\n" ++ (show e)
        --3.a=a 其中a均為元式，則刪除此等式
        else delEqu i es
    --1.2. f = g 其中f為複合子及g是原子，f為原子及g是複合子。
    e@(Func _ _ := Atom _) -> 
        error $ "fail for func = atom:\n" ++ (show e)
    --1.3. f(...) = g(...)  其中f及g 是相異函子。
    e@(Func a as := Func b bs) -> 
        if a /= b  
        then []--error $ "fail for diff func:\n" ++ (show e)
        --1.4. f(a1, a2, ..., aN) = f(b1, b2, ..., bM) 其中N及M數目不同。
        else if length as /= length bs
            then []--error $ "fail for diff func args:\n"++ (show e)
            --4. f(a1, a2, ..., aN) = f(b1, b2, ..., bN) 則用 
            --   ai = bi 等式組取代
            else subequ i (zipWith (\ a b -> a := b) as bs) es
    --2.$X=$X 其中$X均為變數，則刪除此等式
    Var a := Var b  -> if a == b then delEqu i es
                       else es
    --5.t = X 其中 X 為變數而 t為非變數，則以 X = t取代
    t := (Var x)        -> subequ i [Var x := t] es
    e@(Var x := Func w as)  -> 
        --7.X = t 其中 X 為變數而 t為非變數，且 X 在 t 中，
        --  則因正向變數檢查而無法一致化。
        if Var x `elem` as
        then []--error $ "fail for positive var check:\n" ++ (show e)
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
  delEqu i es   = take i es ++ drop (i+1) es

-- sub:: Var -> Term
toSub :: [Equ] -> Term -> Term

toSub equs var = 
    let equTuples = map (\e -> (left e, right e)) equs
    in 
        fromMaybe var (lookup var equTuples)
                
varsub :: [Equ] -> Equ -> [Equ]

varsub oes@(l := r:es) e  = 
    if (l := r) /= e
    then ( (varsub' l e := varsub' r e)):varsub es e
    else oes

varsub [] e = []

-- var sub for the right side in the equation
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

showRule (Fact fact) = showTerm(fact) ++ "." 

main = do
    print $ show $ Atom "atom"

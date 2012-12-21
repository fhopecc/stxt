import Test.HUnit
import Data.Either
import Mobian
import Parser
import GHC.IO.Encoding.CP950
import System.IO
import Control.Monad.Error
import Mobian

main = do
    hSetEncoding stderr cp950
    runTestTT testU

a1 = Atom "甲"
a2 = Atom "乙"
f1 = Func "函" []

testU = test [ "顯示測試" ~: [ 
               "甲" ~=? (show $ Atom "甲"),
               "空函()" ~=? (show $ Func "空函" []),
               "函(甲 乙)" ~=? (show $ Func "函" [Atom "甲", Atom "乙"]),
               "甲=乙" ~=? (show $ Atom "甲" := Atom "乙")
                ],
                "u 函數測試" ~: [ 
                case u (Atom "甲" := Atom "乙") of
                Left  (MobianError err) -> err ~?=
                       "等式「甲=乙」，因其中甲及乙為相異原子，\
                       \一致化失敗!" 
                Right equ -> False ~? "u 甲 = 乙應為錯誤",
                case u (Atom "甲" := Atom "甲") of
                Left  (MobianError err) -> False ~? "u (甲=甲) 應為 []"
                Right equ -> [] ~=? equ,
                case u (Func "函" [] := Atom "甲") of
                Left  (MobianError err) -> err ~?= 
                     "等式「函()=甲」，因其中「函()」為複合子而\
                     \「甲」為原子，一致化失敗!" 
                Right equ -> False ~? "u 函()=甲應為錯誤",
                case u (Func "函甲" [] :=  Func "函乙" []) of
                Left  (MobianError err) -> err ~?= 
                     "等式「函甲()=函乙()」，因其中「函甲()」及「函乙()」\
                     \為相異函，一致化失敗!" 
                Right equ -> False ~? "u 函甲()=函乙()應為錯誤",
                case u (Func "函" [] :=  Func "函" [Atom "甲"]) of
                Left  (MobianError err) -> err ~?= 
                     "等式「函()=函(甲)」，因其中「函()」引數長為0\
                     \而「函(甲)」引數長為1，一致化失敗!" 
                Right equ -> False ~? "u $甲()=函乙()應為錯誤",
                case u (Var "甲" :=  Var "甲") of
                Left  (MobianError err) -> err ~?= 
                     "「u $甲 = $甲」結果應為空串列!" 
                Right equ -> equ ~?= [],
                case u (Var "甲" :=  Var "乙") of
                Left  (MobianError err) -> err ~?= 
                     "「u $甲 = $乙」結果應為「$甲 = $乙」!" 
                Right equ -> equ ~?= [Var "甲" :=  Var "乙"],
                case u (Var "甲" :=  Var "乙") of
                Left  (MobianError err) -> err ~?= 
                     "「u $甲 = $乙」結果應為「$甲 = $乙」!" 
                Right equ -> equ ~?= [Var "甲" :=  Var "乙"],
                case u (Atom "原" :=  Var "變") of
                Left  (MobianError err) -> err ~?= 
                     "「u 原 = $變」結果應為「$變 = 原」!" 
                Right equ -> equ ~?= [(Var "變" :=  Atom "原")]
                ]

              ]

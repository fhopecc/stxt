import Test.HUnit
import STXT

main = runTestTT testParser

testLine = test [ "testLine" ~: 
    [ "test line" ~: 
        [ "\"l1\"" ~=?  run line "l1"
        , "\"l1\"" ~=?  run line "l1\n"
        , "\"l1\"" ~=?  run line "l1\nl2"
        , "\"l1\"" ~=?  run line "l1碼：\n\n"
        , "\"l1\\30908\"" ~=?  run line "l1碼\n"
        , "\"l1\\65306\"" ~=?  run line "l1：\n"
        , "\"l1\\30908\\65306\"" ~=?  run line "l1碼：\n"
        ]
    ,  "test para" ~: 
        [ "Para [\"l1\"]" ~=?  run para "l1"
        , "Para [\"l1\"]" ~=?  run para "l1\n"
        , "Para [\"l1\",\"l2\"]" ~=?  run para "l1\nl2"
        , "Para [\"l1\",\"l2\"]" ~=?  run para "l1\nl2\n"
        , "Para [\"l1\",\"l2\"]" ~=?  run para "l1\nl2碼：\n\n"
        , "Para [\"l1\",\"l2\\30908\\65306\"]" ~=?  run para "l1\nl2碼：\n"
        , "Para [\"l1\"]" ~=?  run para "l1碼：\n\nl2"
        ]
    ]
    ]

testParser = test [ "testParser" ~: 
    [ "test code" ~: 
        [ "Code \"c d\\nc d 2\\nc d 3 \""~=?  run code "碼：\n\nc d\nc d 2\nc d 3 \n\n"
        ]

    , "test sect2title" ~: 
        [ "\"s2\"" ~=?  run sect2title "s2\n-----------\n"
        ]

    , "test sect2" ~: 
        [ "Sect2 \"s2\" []" ~=?  run sect2 "s2\n-----------\n"
        , "Sect2 \"s2\" [Para [\"p1\"]]" ~=?  run sect2 "s2\n-----------\np1"
        , "Sect2 \"s2\" [Para [\"p1\"],Code \"c\"]" ~=?  
          run sect2 "s2\n-----------\np1碼：\n\nc\n\n"

        , "Sect2 \"s2\" [Para [\"p1\"],Code \"c\",Para [\"p2\"]]" ~=?  
          run sect2 "s2\n-----------\np1碼：\n\nc\n\np2"
        ]
    , "test sect2s" ~: 
        [ "[Sect2 \"s2\" []]" ~=?  run sect2s "s2\n--\n"
        , "[Sect2 \"s2\" [Para [\"p1\"]]]" ~=?  run sect2s "s2\n--\np1"
        , "[Sect2 \"t1\" [Para [\"p1\"]],Sect2 \"t2\" "
          ++ "[Para [\"p1\"],Code \"c\"]]" 
          ~=?  run sect2s "t1\n--\np1\n\nt2\n--\np1碼：\n\nc\n\n"
        ]

 
    ]
    ]

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
    ,  "test contents" ~: 
        [ "[Para [\"l1\"],Para [\"l2\"]]" ~=?  
          run contents "l1\n\nl2\n"
        ]
    ]
    ]

testParser = test [ "testParser" ~: 
    [ "test code" ~: 
        [ "Code \"c d\\nc d 2\\nc d 3 \""~=? 
          run code "碼：\n\nc d\nc d 2\nc d 3 \n\n\n"
        ]

    , "test sect2title" ~: 
        [ "\"s2\"" ~=?  run sect2title "s2\n-----------\n\n"
        ]
    , "test sect2" ~: 
        [ "Sect2 \"s2\" []" ~=?  run sect2 "s2\n--\n\n"
        , "Sect2 \"s2\" [Para [\"p1\"]]" ~=?  run sect2 "s2\n--\n\np1"
        , "Sect2 \"s2\" [Para [\"p1\"],Code \"c\"]" ~=?  
          run sect2 "s2\n--\n\np1碼：\n\nc\n\n\n"

        , "Sect2 \"s2\" [Para [\"p1\"],Code \"c\",Para [\"p2\"]]" ~=?  
          run sect2 "s2\n--\n\np1碼：\n\nc\n\n\np2"
        ]
    , "test sect2s" ~: 
        [ "[Sect2 \"s2\" []]" ~=?  run sect2s "s2\n--\n\n"
        , "[Sect2 \"s2\" [Para [\"p1\"]]]" ~=?  run sect2s "s2\n--\n\np1"
        , "[Sect2 \"t1\" [Para [\"p1\"]],Sect2 \"t2\" "
          ++ "[Para [\"p1\"],Code \"c\"]]" 
          ~=?  run sect2s "t1\n--\n\np1\nt2\n--\n\np1碼：\n\nc\n\n\n"
        , "[Sect2 \"t1\" [Para [\"l1\",\"l2\"]],Sect2 \"t2\" "
          ++ "[Para [\"l1\"]]]" 
          ~=?  run sect2s "t1\n--\n\nl1\nl2\nt2\n--\n\nl1"
        ]
    , "test sect1" ~: 
        [ "Sect1 \"s1\" [Sect2 \"s2\" []]" ~=? 
          run sect1 "s1\n==\n\ns2\n--\n\n"
        , "Sect1 \"s1\" [Sect2 \"s2\" [Para [\"l1\",\"l2\"],Code \"c1\\nc2\\n c3\"]]" ~=? 
          run sect1 "s1\n==\n\ns2\n--\n\nl1\nl2碼：\n\nc1\nc2\n c3\n\n\n"
        ]
    , "test sect1s" ~: 
        [ "[Sect1 \"s1\" [Sect2 \"s2\" []]]" ~=? 
          run sect1s "s1\n==\n\ns2\n--\n\n"
        , "[Sect1 \"s1\" [Sect2 \"t1\" [Para [\"l1\"]]],Sect1 \"s2\" []]" ~=? 
          run sect1s "s1\n==\n\nt1\n--\n\nl1\n\ns2\n==\n\n"

        ,  "[Sect1 \"c1\" [Sect2 \"t1\" [Para [\"l1\"]]],"
       ++  "Sect1 \"c2\" []]" 
       ~=? 
          run sect1s "c1\n==\n\nt1\n--\n\nl1\n\nc2\n==\n\n"
        ]
    ]
    ]

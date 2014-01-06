import Test.HUnit
import STXT

main = runTestTT testParser

testParser = test [ "testParser" ~: 
    [ "test para" ~: 
        [ "[\"line1\"]" ~=?  run para "line1"
        , "[\"line1\"]" ~=?  run para "line1\n"
        , "[\"line1\",\"line2\"]" ~=?  run para "line1\nline2"
        , "[\"line1\",\"line2\"]" ~=?  run para "line1\nline2\n"
        ]

    , "test paras" ~: 
        [ "[]"            ~=?  run paras ""
        , "[]"            ~=?  run paras "\n"
        , "[[\"line1\"]]" ~=?  run paras "line1"
        , "[[\"line1\"]]" ~=?  run paras"line1\n"
        , "[[\"line1\",\"line2\"]]" ~=?  run paras "line1\nline2"
        , "[[\"line1\",\"line2\"]]" ~=?  run paras "line1\nline2\n"
        , "[[\"p1\"],[\"p2\"]]" ~=?  run paras "p1\n\np2\n"
        , "[[\"p1\"],[\"p2\"]]" ~=?  run paras "p1\n\n\np2\n\n"
        ]

    , "test sect1title" ~: 
        [ "\"s1\"" ~=?  run sect1title "s1\n===========\n"
        ]

    , "test sect1" ~: 
        [ "\"s1\"" ~=?  run sect1title "s1\n===========\n"
        ]
 
    ]
    ]

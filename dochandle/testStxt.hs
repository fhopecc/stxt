import Test.HUnit
import STXT

main = runTestTT testParaObj

testParaObj = test [ "test ParaObj" ~:
    [ "test Link" ~:
        [ "Link \"t\" \"url\"" ~=? rawRunPart paraLink "[t|url]"
        , "Link \"t\" \"url\"" ~=? rawRunPart paraObj "[t|url]"
        , "Str \"str\"" ~=? rawRunPart paraObj "str[t|url]"
        ]
    , "test ParaObjs" ~:
        [ "Str \"str\"" ~=? rawRunPart paraObj "str[t|url]"
        , "[Str \"str\",Link \"t\" \"url\"]" ~=? 
           rawRunPart paraObjs "str[t|url]"
        , "[Str \"s 1\",Link \"t\" \"url\",Str \"s2\"]" ~=? 
           rawRunPart paraObjs "s 1[t|url]s2"
        ]
    ]]

testLine = test [ "testLine" ~: 
    [ "test line" ~: 
        [ "\"l1\"" ~=?  rawRunPart line "l1"
        , "\"l1\"" ~=?  rawRunPart line "l1\n"
        , "\"l1\"" ~=?  rawRunPart line "l1\nl2"
        , "\"l1\"" ~=?  rawRunPart line "l1碼：\n\n"
        , "\"l1\\30908\"" ~=?  rawRunPart line "l1碼\n"
        , "\"l1\\65306\"" ~=?  rawRunPart line "l1：\n"
        , "\"l1\\30908\\65306\"" ~=?  rawRunPart line "l1碼：\n"
        ]

    ,  "test para" ~: 
        [ "Para [\"l1\"]" ~=?  rawRunPart para "l1"
        , "Para [\"l1\"]" ~=?  rawRunPart para "l1\n"
        , "Para [\"l1\",\"l2\"]" ~=?  rawRunPart para "l1\nl2"
        , "Para [\"l1\",\"l2\"]" ~=?  rawRunPart para "l1\nl2\n"
        , "Para [\"l1\",\"l2\"]" ~=?  rawRunPart para "l1\nl2碼：\n\n"
        , "Para [\"l1\",\"l2\\30908\\65306\"]" ~=?  rawRunPart para "l1\nl2碼：\n"
        , "Para [\"l1\"]" ~=?  rawRunPart para "l1碼：\n\nl2"
        ]
    ,  "test contents" ~: 
        [ "[Para [\"l1\"],Para [\"l2\"]]" ~=?  
          rawRunPart contents "l1\n\nl2\n"
        ]
    ]
    ]

testParser = test [ "testParser" ~: 
    [ "test code" ~: 
        [ "Code \"c d\\nc d 2\\nc d 3 \""~=? 
          rawRunPart code "：\n\nc d\nc d 2\nc d 3 \n\n\n"
        ]

    , "test sect2title" ~: 
        [ "\"s2\"" ~=?  rawRunPart sect2title "s2\n..\n\n"
        ]
    , "test sect2" ~: 
        [ "Sect2 (0,0) \"s2\" []" ~=?  rawRunPart sect2 "s2\n..\n\n"
        , "Sect2 (0,0) \"s2\" [Para [\"p1\"]]" ~=?  rawRunPart sect2 "s2\n..\n\np1"
        , "Sect2 (0,0) \"s2\" [Para [\"p1\"],Code \"c\"]" ~=?  
          rawRunPart sect2 "s2\n..\n\np1：\n\nc\n\n\n"

        , "Sect2 (0,0) \"s2\" [Para [\"p1\"],Code \"c\",Para [\"p2\"]]" ~=?  
          rawRunPart sect2 "s2\n..\n\np1：\n\nc\n\n\np2"
        ]
    , "test sect2s" ~: 
        [ "[Sect2 (0,0) \"s2\" []]" ~=?  rawRunPart sect2s "s2\n..\n\n"
        , "[Sect2 (0,0) \"s2\" [Para [\"p1\"]]]" ~=?  rawRunPart sect2s "s2\n..\n\np1"
        , "[Sect2 (0,0) \"t1\" [Para [\"p1\"]],Sect2 (0,0) \"t2\" "
          ++ "[Para [\"p1\"],Code \"c\"]]" 
          ~=?  rawRunPart sect2s "t1\n..\n\np1\nt2\n..\n\np1：\n\nc\n\n\n"
        , "[Sect2 (0,0) \"t1\" [Para [\"l1\",\"l2\"]],Sect2 (0,0) \"t2\" "
          ++ "[Para [\"l1\"]]]" 
          ~=?  rawRunPart sect2s "t1\n..\n\nl1\nl2\nt2\n..\n\nl1"
        ]
    , "test sect1" ~: 
        [ "Sect1 0 \"s1\" [Sect2 (0,0) \"s2\" []]" ~=? 
          rawRunPart sect1 "s1\n--\n\ns2\n..\n\n"
        , "Sect1 0 \"s1\" [Sect2 (0,0) \"s2\" [Para [\"l1\",\"l2\"],Code \"c1\\nc2\\n c3\"]]" ~=? 
          rawRunPart sect1 "s1\n--\n\ns2\n..\n\nl1\nl2：\n\nc1\nc2\n c3\n\n\n"
        ]
    , "test sect1s" ~: 
        [ "[Sect1 0 \"s1\" [Sect2 (0,0) \"s2\" []]]" ~=? 
          rawRunPart sect1s "s1\n--\n\ns2\n..\n\n"
        , "[Sect1 0 \"s1\" [Sect2 (0,0) \"t1\" [Para [\"l1\"]]],Sect1 0 \"s2\" []]" ~=? 
          rawRunPart sect1s "s1\n--\n\nt1\n..\n\nl1\n\ns2\n--\n\n"

        ,  "[Sect1 0 \"c1\" [Sect2 (0,0) \"t1\" [Para [\"l1\"]]],"
       ++  "Sect1 0 \"c2\" []]" 
       ~=? 
          rawRunPart sect1s "c1\n--\n\nt1\n..\n\nl1\n\nc2\n--\n\n"
        ]
    ]]


testNumber = test [ "testNumber" ~: 
    [ "test number" ~: 
        [ "Doc \"doc\" [Sect1 1 \"s1\" []]"~=? 
          runPart doc "doc\n==\n\ns1\n--\n\n"
        , "Doc \"doc\" [Sect1 1 \"s1\" [],Sect1 2 \"s2\" []]" ~=? 
          runPart doc "doc\n==\n\ns1\n--\n\ns2\n--\n\n"
        , "Doc \"doc\" [Sect1 1 \"s1\" [Sect2 (1,1) \"t1\" []]]" ~=? 
          runPart doc "doc\n==\n\ns1\n--\n\nt1\n..\n\n"
        ]

    ]] 

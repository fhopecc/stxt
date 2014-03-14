import Test.HUnit
import STXT

main = runTestTT testParser

testParser = test [ "test ParaObj" ~:
    [ "test line" ~: 
        [ "\"l1\"" ~=?  rawRunPart line "l1"
        , "\"l1\"" ~=?  rawRunPart line "l1\n"
        , "\"l1\"" ~=?  rawRunPart line "l1\nl2"
        , "\"l1\\65306\"" ~=?  rawRunPart line "l1：\n\n"
        , "\"l1\\65306\"" ~=?  rawRunPart line "l1：\n"
        ]
    , "test Link" ~:
        [ "Str \"[t|url]\"" ~=? rawRunPart paraLink "[t|url]"
        , "ILink \"t\"" ~=? rawRunPart paraLink "[t]"
        , "Link \"t\" \"http://www.hltb.gov.tw\"" ~=? rawRunPart paraObj "[t|http://www.hltb.gov.tw]"
        , "Str \"str\"" ~=? rawRunPart paraObj "str[t|url]"
        ]
    , "test paraObj" ~:
        [ "Str \"str\"" ~=? rawRunPart paraObj "str[t|url]"
        , "[Str \"str\",Link \"t\" \"http://www.hltb.gov.tw\"]" ~=? 
           rawRunPart paraObjs "str[t|http://www.hltb.gov.tw]"
        , "[Str \"s 1\",Str \"[t|url]\",Str \"s2\"]" ~=? 
           rawRunPart paraObjs "s 1[t|url]s2"
        ]
    ,  "test para" ~: 
        [ "Para [Str \"l1\"]" ~=?  rawRunPart para "l1"
        , "Para [Str \"l1\"]" ~=?  rawRunPart para "l1\n"
        , "Para [Str \"l1l2\"]" ~=?  rawRunPart para "l1\nl2"
        , "Para [Str \"l1l2\"]" ~=?  rawRunPart para "l1\nl2\n"
        , "Para [Str \"l1l2\\65306\"]" ~=?  rawRunPart para "l1\nl2：\n\n"
        , "Para [Str \"l1l2\\65306\"]" ~=?  rawRunPart para "l1\nl2：\n"
        , "Para [Str \"l1\\65306\"]" ~=?  rawRunPart para "l1：\n\nl2"
        ]
    , "test codePara" ~: 
        [ "Code \"c d\\nc d 2\\nc d 3 \""~=? 
          rawRunPart codePara "：\n\nc d\nc d 2\nc d 3 \n\n"
        ]
    , "test codeParas" ~: 
        [ "Code \"c1\\n\\nc2\\n\\nc3\"" ~=?  
          rawRunPart codeParas "：：\n\nc1\n\nc2\n\nc3\n\n\n"
        ]
    ,  "test content" ~: 
        [ "[Para [Str \"l1\"],Para [Str \"l2\"]]" ~=?  
          rawRunPart content "l1\n\nl2\n"
        ]
    , "test sect2title" ~: 
        [ "\"s2\"" ~=?  rawRunPart sect2title "s2\n..\n\n"
        ]
    , "test sect2" ~: 
        [ "Sect2 (0,0) \"s2\" []" ~=?  rawRunPart sect2 "s2\n..\n\n"
        , "Sect2 (0,0) \"s2\" [Para [Str \"p1\"]]" ~=?  rawRunPart sect2 "s2\n..\n\np1"
        , "Sect2 (0,0) \"s2\" [Para [Str \"p1\\65306\"],Code \"c\"]" ~=?  
          rawRunPart sect2 "s2\n..\n\np1：\n\nc\n\n\n"

        , "Sect2 (0,0) \"s2\" [Para [Str \"p1\\65306\"],Code \"c\",Para [Str \"p2\"]]" ~=?  
          rawRunPart sect2 "s2\n..\n\np1：\n\nc\n\np2"
        ]
    , "test sect2s" ~: 
        [ "[Sect2 (0,0) \"s2\" []]" ~=?  rawRunPart sect2s "s2\n..\n\n"
        , "[Sect2 (0,0) \"s2\" [Para [Str \"p1\"]]]" ~=?  rawRunPart sect2s "s2\n..\n\np1"
        , "[Sect2 (0,0) \"t1\" [Para [Str \"p1\"]],Sect2 (0,0) \"t2\" "
          ++ "[Para [Str \"p1\\65306\"],Code \"c\"]]" 
          ~=?  rawRunPart sect2s "t1\n..\n\np1\nt2\n..\n\np1：\n\nc\n\n\n"
        , "[Sect2 (0,0) \"t1\" [Para [Str \"l1l2\"]],Sect2 (0,0) \"t2\" "
          ++ "[Para [Str \"l1\"]]]" 
          ~=?  rawRunPart sect2s "t1\n..\n\nl1\nl2\nt2\n..\n\nl1"
        ]
    , "test sect1" ~: 
        [ "Sect1 0 \"s1\" [] [Sect2 (0,0) \"s2\" []]" ~=? 
          rawRunPart sect1 "s1\n--\n\ns2\n..\n\n"
        , "Sect1 0 \"s1\" [Para [Str \"p1\"]] [Sect2 (0,0) \"s2\" [Para [Str \"l1l2\\65306\"],Code \"c1\\nc2\\n c3\"]]" ~=? 
          rawRunPart sect1 "s1\n--\n\np1\ns2\n..\n\nl1\nl2：\n\nc1\nc2\n c3\n\n\n"
        ]
    , "test sect1s" ~: 
        [ "[Sect1 0 \"s1\" [] [Sect2 (0,0) \"s2\" []]]" ~=? 
          rawRunPart sect1s "s1\n--\n\ns2\n..\n\n"
        , "[Sect1 0 \"s1\" [] [Sect2 (0,0) \"t1\" [Para [Str \"l1\"]]],Sect1 0 \"s2\" [] []]" ~=? 
          rawRunPart sect1s "s1\n--\n\nt1\n..\n\nl1\n\ns2\n--\n\n"

        ,  "[Sect1 0 \"c1\" [] [Sect2 (0,0) \"t1\" [Para [Str \"l1\"]]],"
       ++  "Sect1 0 \"c2\" [] []]" 
       ~=? 
          rawRunPart sect1s "c1\n--\n\nt1\n..\n\nl1\n\nc2\n--\n\n"
        ,  "[Sect1 0 \"c1\" [] [Sect2 (0,0) \"t1\" [Para [Str \"l1\"]]],"
       ++  "Include \"i\",Sect1 0 \"c2\" [] []]" 
       ~=? 
          rawRunPart sect1s "c1\n--\n\nt1\n..\n\nl1\n\n<i>\n\nc2\n--\n\n"
        ]

        , "test include" ~: 
        [ "Include \"d:\\\\dir\\\\file.txt\"" ~=? rawRunPart include "<d:\\dir\\file.txt>"
        ]
        , "test doc" ~: 
        [ "Doc \"doc\" [] [Sect1 1 \"s1\" [] []]" ~=? 
          runPart doc "doc\n==\n\ns1\n--\n\n"
        , "Doc \"doc\" [] [Sect1 1 \"s1\" [] [],Sect1 2 \"s2\" [] []]" ~=? 
          runPart doc "doc\n==\n\ns1\n--\n\ns2\n--\n\n"
        , "Doc \"doc\" [] [Sect1 1 \"s1\" [] [Sect2 (1,1) \"t1\" []]]" ~=? 
          runPart doc "doc\n==\n\ns1\n--\n\nt1\n..\n\n"
        , "Doc \"doc\" [Para [Str \"l1l2\"]] [Sect1 1 \"s1\" [] []]"~=? 
          runPart doc "doc\n==\n\nl1\nl2\n\ns1\n--\n\n"
        ]
        , "test accessNode" ~: 
        [ "Just (Sect1 0 \"s1\" [] [])" ~=? 
          show ((rawRun "doc\n==\n\ns1\n--\n\n") `getSect1` "s1")
        , "Just (Sect2 (0,0) \"t2\" [])" ~=? 
          show ((rawRun "doc\n==\n\ns1\n--\n\nt1\n..\n\nt2\n..\n\n") 
                `getSect2` "t2")
        ]

    ]]

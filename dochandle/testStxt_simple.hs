import Test.HUnit
import STXT

main = runTestTT testParser

testParser = test [ "testParser" ~: 
    [ "test title" ~: "Title [\"title\"]" ~=?  
       run title "title\n--------" 

    , "test mutiline title" ~: "Title [\"title\",\"title2\"]" ~=?  
       run title "title\ntitle2\n--------" 

    , "test num" ~: "11" ~=? run num "11" 

    , "test nums" ~: "[11,4]" ~=? run nums "11.4." 

    , "test ntitle" ~: "NTitle [11,4] \"title\"" ~=?  
       run ntitle "11.4.title" 
 
    , "test para" ~: 
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

    , "test section" ~: 
        [ "Section (NTitle [1] \"title\") []" ~=?  run section "1.title"
        , "Section (NTitle [1] \"t\") [[\"p1\"]]" ~=?  run section "1.t\np1"
        , "Section (NTitle [1] \"t\") [[\"p1\"]]" ~=?  
          run section "1.t\np1\n"
        , "Section (NTitle [1] \"t\") [[\"p1\"],[\"p2\"]]" ~=?  
          run section "1.t\np1\n\np2\n"
        ]

    , "test sections" ~: 
        [ "[]"            ~=? run sections ""
        , "[Section (NTitle [1] \"t\") []]" ~=? run sections "1.t"
        , "[Section (NTitle [1] \"t\") []]" ~=? run sections "1.t\n"
        , "[Section (NTitle [1] \"t\") [],Section (NTitle [2] \"t\") []]" 
        ~=? run sections "1.t\n2.t"
        , "[Section (NTitle [1] \"t\") [],Section (NTitle [2] \"t\") []]" 
        ~=? run sections "1.t\n2.t\n"
        , "[Section (NTitle [1] \"t\") [[\"p1\"]],Section (NTitle [2] \"t\") []]" 
        ~=? run sections "1.t\np1\n2.t\n"
        , "[Section (NTitle [1] \"t\") [[\"p1\"]],Section (NTitle [2] \"t\") [[\"p2\"]]]" 
        ~=? run sections "1.t\np1\n\n2.t\np2\n"
        ]
    , "test document" ~: 
        [ "Document (Title [\"t\"]) []" ~=? run document "t\n-\n\n"
        , "Document (Title [\"t\"]) [Section (NTitle [1] \"s\") []]" ~=? run document "t\n-\n1.s\n"
        ]
    ]
    ]

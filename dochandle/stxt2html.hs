import qualified STXT
import Text.Html
import Control.Monad.Reader
import System.IO
import Data.List

main = do 
    let src = "d:\\stxt\\doc\\vbscript\\vbscript.txt"
    f <- openFile src ReadMode
    hSetEncoding f utf8
    c <- hGetContents f
    
    mapM_ writeHtml (runReader askHtmls (Env { getSource = src
                                       , getOutDir = "d:\\stxt\\fhopeccweb\\vbscript"
                                       , getDoc = STXT.run c
                                       }))

writeHtml :: (FilePath, Html) -> IO ()
writeHtml (path, html) = do
    f <- openFile path WriteMode
    hSetEncoding f utf8
    hPutStr f (renderHtml html)
    putStrLn path
    hFlush f
    hClose f

askHtmls :: Reader Env [(FilePath, Html)]
askHtmls = do
    ihtml <- askIndexHtml
    s1htmls <- askSect1Htmls  
    s2htmls <- askSect2Htmls
    return $ ihtml : s1htmls ++ s2htmls

askSect1Htmls :: Reader Env [(FilePath, Html)]
askSect1Htmls = do
    s1s <- askSect1s
    mapM askSect1Html s1s

askSect2Htmls ::  Reader Env [(FilePath, Html)]
askSect2Htmls = do
    s1s <- askSect1s
    mapM askSect2Html $ concat [s2s | s1@(STXT.Sect1 _ _ s2s) <- s1s]


askSect1Html :: STXT.Sect1 -> Reader Env (FilePath, Html)
askSect1Html s1@(STXT.Sect1 n t s2s) = do
    doc@(STXT.Doc _ s1s) <- askDoc
    sect1bar <- askSect1Bar s1
    sect2bar <- askSect2Bar (head s2s)
    let html = thehtml ! [lang "zh-tw"] 
                << header 
                    << myMeta
                   +++ myLink
                   +++ thetitle << t
               +++ body 
                    << sect1bar
                   +++ sect2bar
                   +++ rightAds
    s1File <- askSect1File s1
    return $ (s1File, html)

askSect2Html :: STXT.Sect2 -> Reader Env (FilePath, Html)
askSect2Html s2@(STXT.Sect2 (n1, n2) t cs) = do
    doc@(STXT.Doc _ s1s) <- askDoc
    s1 <- askSect1OfSect2 s2
    sect1bar <- askSect1Bar s1
    sect2bar <- askSect2Bar s2
    let html = thehtml ! [lang "zh-tw"] 
                << header 
                    << myMeta
                   +++ myLink
                   +++ thetitle << t
               +++ body 
                    << sect1bar
                   +++ sect2bar
                   +++ rightAds
                   +++ thediv ![identifier "content"]
                        << map content2Html cs
    file <- askSect2File s2
    return $ (file, html)

askSect1OfSect2 :: STXT.Sect2 -> Reader Env STXT.Sect1
askSect1OfSect2 s2@(STXT.Sect2 (n1, n2) _ _) = do
    s1s <- askSect1s
    return $ maybe (head s1s) id (find (\s1@(STXT.Sect1 n _ _) -> n == n1) s1s)


data Env = Env { getSource :: FilePath  
               , getOutDir :: FilePath  
               , getDoc    :: STXT.Doc
               }

content2Html :: STXT.Content -> Html
content2Html (STXT.Para ls) = thediv ! [theclass "para"]
                                << paragraph << concat ls 
content2Html (STXT.Code c) = pre << c

askIndexHtml :: Reader Env (FilePath, Html)
askIndexHtml = do
    t <- askDocTitle
    doc@(STXT.Doc _ s1s) <- askDoc
    sect1bar <- askSect1Bar $ head s1s
    let s1@(STXT.Sect1 _ _ s2s) = head s1s
    sect2bar <- askSect2Bar (head s2s)
    let html = thehtml ! [lang "zh-tw"] 
                << header 
                    << myMeta
                   +++ myLink
                   +++ thetitle << t
               +++ body 
                    << sect1bar 
                   +++ sect2bar 
                   +++ rightAds

    f <- askIndexPath
    return (f, html)

myMeta = meta ! [ httpequiv "Content-Type"
                , content   "text/html; charset=utf-8"
                ] 

myLink = thelink ! [ rel  "stylesheet"
                   , href "web.css" 
                   ] << noHtml

askIndexPath :: Reader Env String
askIndexPath = do
    outdir <- askOutDir 
    return $ outdir ++ "\\" ++ "index.html"

askSect1File :: STXT.Sect1 -> Reader Env String
askSect1File s1@(STXT.Sect1 n t _) = do
    outdir <- askOutDir 
    return $ outdir ++ "\\" ++ show n ++ ".html" 

askSect2File :: STXT.Sect2 -> Reader Env String
askSect2File s2@(STXT.Sect2 (n1, n2) t _) = do
    outdir <- askOutDir 
    return $ outdir ++ "\\" ++ sect2Path s2

askSect1Bar :: STXT.Sect1 -> Reader Env Html
askSect1Bar (STXT.Sect1 selected t _) = do
    s1s <- askSect1s 
    return $ thediv ! [identifier "sect1_bar"] 
                << table 
                    << besides [ (label (n==selected)) << sect1Anchor s1 
                               | s1@(STXT.Sect1 n _ _) <- s1s
                               ]

askSect2Bar :: STXT.Sect2 -> Reader Env Html
askSect2Bar s2@(STXT.Sect2 (sn1, sn2) _ _) = do
    s1  <- askSect1OfSect2 s2
    s2s <- askSect2s s1 
    return $ thediv ! [identifier "sect2_bar"] 
                << table 
                    << aboves [ (label (n1 == sn1 && n2 == sn2 )) << sect2Anchor s2 
                              | s2@(STXT.Sect2 (n1,n2) _ _) <- s2s
                              ]

label :: Bool -> Html -> Html
label selected child = (if selected then
        td ! [theclass "selected"]   
    else 
        td) << child


sect1Anchor :: STXT.Sect1 -> Html
sect1Anchor s1@(STXT.Sect1 n t _) = 
    anchor ! [href (sect1Path s1)] << t

sect2Anchor :: STXT.Sect2 -> Html
sect2Anchor s2@(STXT.Sect2 _ t _) = 
    anchor ! [href (sect2Path s2)] << t


sect1Path :: STXT.Sect1 -> String
sect1Path s1@(STXT.Sect1 n t _) = show n ++ ".html"

sect2Path :: STXT.Sect2 -> String
sect2Path s1@(STXT.Sect2 (n1, n2) t _) = 
    show n1 ++ "_" ++ show n2 ++ ".html"

askOutDir :: Reader Env String
askOutDir = do
    env <- ask
    return $ getOutDir env

askDoc :: Reader Env STXT.Doc
askDoc = do
    env <- ask
    return $ getDoc env

askDocTitle :: Reader Env String
askDocTitle = do 
    (STXT.Doc t _) <- askDoc
    return t

askSect1s :: Reader Env STXT.Sect1s
askSect1s = do 
    (STXT.Doc _ s1s) <- askDoc
    return s1s

askSect2s :: STXT.Sect1 -> Reader Env STXT.Sect2s
askSect2s (STXT.Sect1 _ _ s2s) = do 
    return s2s

rightAds :: Html
rightAds = thediv ! [identifier "rightbar"]
            << [ tag "script"  ! [thetype "text/javascript"] 
                 << (primHtml $ concat [ "<!--\n"
                                       , "google_ad_client = \"pub-7516968926110807\";\n"
                                       , "/* Structed text left vertical unit */\n"
                                       , "google_ad_slot = \"5496674888\";\n"
                                       , "google_ad_width = 120;\n"
                                       , "google_ad_height = 600;\n"
                                       , "//-->\n"
                                       ])
               , tag "script" ! [ thetype "text/javascript"
                               , src "http://pagead2.googlesyndication.com/pagead/show_ads.js"
                               ] << noHtml
               ]
           

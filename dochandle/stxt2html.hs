import qualified STXT
import Text.Html
import Control.Monad.Reader
import Control.Monad.State
import System.Environment 
import System.IO
import qualified System.FilePath.Windows as FP
import Data.List
import Data.Maybe
import System.Directory

data Env = Env { getSource :: FilePath  
               , getOutDir :: FilePath  
               , getDoc    :: STXT.Doc
               }

data Page = Page { pSource  :: FilePath
                 , pOutDir  :: FilePath
                 , pDoc     :: STXT.Doc
                 , pTitle   :: String
                 , pContent :: STXT.Content
                 , pSect1s  :: STXT.Sect1s
                 , pSect1   :: Maybe STXT.Sect1
                 , pSect2s  :: STXT.Sect2s
                 , pSect2   :: Maybe STXT.Sect2
                 } 

siteUrl = "http://fhopehltb.appspot.com"
siteName = "剛的網站"
webdir = "d:\\stxt\\fhopecc\\www"
webcss = "d:\\stxt\\dochandle\\web.css"
index  = "d:\\stxt\\fhopecc\\www\\index\\index.html"

main = do 
    --let src = "d:\\stxt\\doc\\vbscript\\vbscript.txt"
    args <- getArgs 
    let src = args !! 0
    let basename = FP.takeBaseName src
    let outDir = FP.combine webdir basename
    createDirectoryIfMissing True outDir
    putStr "outDir="
    putStrLn $ outDir 
    f <- openFile src ReadMode
    hSetEncoding f utf8
    c <- hGetContents f
    copyFile webcss (FP.combine outDir "web.css")
    putStrLn "copy web.css"
    mapM_ writeHtml (runState getHtmls (Page  { getSource = src
                                             , getOutDir = outDir
                                             , getDoc = STXT.run c
                                             }))
    copyFile index "d:\\stxt\\fhopecc\\www\\index.html"

getHtmls :: State Page [(FilePath, Html)]
getHtmls = do
    ihtml <- askIndexHtml
    s1htmls <- askSect1Htmls  
    s2htmls <- askSect2Htmls
    return $ ihtml : s1htmls ++ s2htmls

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

setIndexPage :: State Page ()
setIndexPage = do
    s  <- get
    (STXT.Doc t cs s1s) <- pDoc s
    put s { pTitle   = t
          , pContent = cs
          , pSect1   = Nothing
          , pSect2   = Nothing
          , pSect1s  = s1s
          , pSect2s  = []
          }

getSect1s = do
    p <- get
    return $ pSect1s p

getSect1 = do
    p <- get
    return $ pSect1 p  

getSect2s = do
    p <- get
    return $ pSect2s p  

getSect2 = do
    p <- get
    return $ pSect2 p

getContent = do
    p <- get
    return $ pContent p  

getTitle = do
    p <- get
    return $ pTitle p  

getIndexFile :: State Page String
getIndexFile = do
    outdir <- getOutDir 
    return $ outdir ++ "\\" ++ "index.html"

getTitleBar = do
    title <- getTitle 
    return $ thediv ! [identifier "title_bar"] 
                << table 
                    << besides [ td << anchor ! [href siteUrl] 
                                        << siteName  
                               , td << title
                               ]


getSect1Bar :: State Page Html
getSect1Bar = do
    s1s <- getSect1s
    s1  <- getSect1
    return $ if isJust s1 then do
                let (STXT.Sect1 selected _ _ _) = fromJust s1
                thediv ! [identifier "sect1_bar"] 
                    << table 
                        << besides [(label (n==selected)) << sect1Anchor s1 
                                   | s1@(STXT.Sect1 n _ _ _) <- s1s
                                   ]
              else toHtml ""

getSect2Bar :: State Page Html
getSect2Bar = do
    Page { pSect1  = s1
         , pSect2s = s2s 
         , pSect2  = ss2
         } <- get

    return $ thediv ! [identifier "sect2_bar"] 
                << table 
                    << aboves [ (label (s2 == selected )) << sect2Anchor s2 
                              | s2 <- s2s
                              ]

getPageHtml :: State Page (FilePath, Html)
getPageHtml = do
    title    <- getTitle
    titlebar <- getTitleBar
    sect1bar <- getSect1Bar 
    sect2bar <- getSect2Bar
    content  <- getContent
    let html = thehtml ! [lang "zh-tw"] 
                << header 
                    << myMeta
                   +++ myLink
                   +++ thetitle << title
               +++ body 
                    << titlebar
                   +++ sect1bar 
                   +++ sect2bar 
                   +++ rightAds
                   +++ thediv ![identifier "content"]
                        << map elem2Html content
    f <- getIndexFile
    return (f, html)

getIndexHtml :: Reader Env (FilePath, Html)
getIndexHtml = do
    t <- askDocTitle
    doc@(STXT.Doc _ cs s1s) <- askDoc
    titlebar <- askTitleBar 
    sect1bar <- if null s1s then 
                   return $ toHtml "" 
                else askSect1Bar $ head s1s
    sect2bar <- if null s1s then 
                   return $ toHtml "" 
                else do 
                    let s1@(STXT.Sect1 _ _ _ s2s) = head s1s
                    askSect2Bar (head s2s)

    let html = thehtml ! [lang "zh-tw"] 
                << header 
                    << myMeta
                   +++ myLink
                   +++ thetitle << t
               +++ body 
                    << titlebar
                   +++ sect1bar 
                   +++ sect2bar 
                   +++ rightAds
                   +++ thediv ![identifier "content"]
                        << map elem2Html cs
    f <- askIndexFile
    return (f, html)



askIndexHtml :: Reader Env (FilePath, Html)
askIndexHtml = do
    t <- askDocTitle
    doc@(STXT.Doc _ cs s1s) <- askDoc
    titlebar <- askTitleBar 
    sect1bar <- if null s1s then 
                   return $ toHtml "" 
                else askSect1Bar $ head s1s
    sect2bar <- if null s1s then 
                   return $ toHtml "" 
                else do 
                    let s1@(STXT.Sect1 _ _ _ s2s) = head s1s
                    askSect2Bar (head s2s)

    let html = thehtml ! [lang "zh-tw"] 
                << header 
                    << myMeta
                   +++ myLink
                   +++ thetitle << t
               +++ body 
                    << titlebar
                   +++ sect1bar 
                   +++ sect2bar 
                   +++ rightAds
                   +++ thediv ![identifier "content"]
                        << map elem2Html cs
    f <- askIndexFile
    return (f, html)

askSect1Htmls :: Reader Env [(FilePath, Html)]
askSect1Htmls = do
    s1s <- askSect1s
    mapM askSect1Html s1s

askSect2Htmls ::  Reader Env [(FilePath, Html)]
askSect2Htmls = do
    s1s <- askSect1s
    mapM askSect2Html $ concat [s2s | s1@(STXT.Sect1 _ _ _ s2s) <- s1s]

askSect1Html :: STXT.Sect1 -> Reader Env (FilePath, Html)
askSect1Html s1@(STXT.Sect1 n t cs s2s) = do
    doc@(STXT.Doc _ _ s1s) <- askDoc
    titlebar <- askTitleBar
    sect1bar <- askSect1Bar s1
    sect2bar <- askSect2Bar (head s2s)
    let html = thehtml ! [lang "zh-tw"] 
                << header 
                    << myMeta
                   +++ myLink
                   +++ thetitle << t
               +++ body 
                    << titlebar
                   +++ sect1bar
                   +++ sect2bar
                   +++ rightAds
                   +++ thediv ![identifier "content"]
                        << map elem2Html cs

    s1File <- askSect1File s1
    return $ (s1File, html)

askSect2Html :: STXT.Sect2 -> Reader Env (FilePath, Html)
askSect2Html s2@(STXT.Sect2 (n1, n2) t cs) = do
    doc@(STXT.Doc _ _ s1s) <- askDoc
    s1 <- askSect1OfSect2 s2
    titlebar <- askTitleBar
    sect1bar <- askSect1Bar s1
    sect2bar <- askSect2Bar s2
    let html = thehtml ! [lang "zh-tw"] 
                << header 
                    << myMeta
                   +++ myLink
                   +++ thetitle << t
               +++ body 
                    << titlebar
                   +++ sect1bar
                   +++ sect2bar
                   +++ rightAds
                   +++ thediv ![identifier "content"]
                        << map elem2Html cs
    file <- askSect2File s2
    return $ (file, html)

askSect1OfSect2 :: STXT.Sect2 -> Reader Env STXT.Sect1
askSect1OfSect2 s2@(STXT.Sect2 (n1, n2) _ _) = do
    s1s <- askSect1s
    return $ maybe (head s1s) id (find (\s1@(STXT.Sect1 n _ _ _) -> n == n1) s1s)


askIndexFile :: Reader Env String
askIndexFile = do
    outdir <- askOutDir 
    return $ outdir ++ "\\" ++ "index.html"

askSect1File :: STXT.Sect1 -> Reader Env String
askSect1File s1@(STXT.Sect1 n t _ _) = do
    outdir <- askOutDir 
    return $ outdir ++ "\\" ++ show n ++ ".html" 

askSect2File :: STXT.Sect2 -> Reader Env String
askSect2File s2@(STXT.Sect2 (n1, n2) t _) = do
    outdir <- askOutDir 
    return $ outdir ++ "\\" ++ sect2Path s2

askTitleBar :: Reader Env Html
askTitleBar = do
    t <- askDocTitle 
    return $ thediv ! [identifier "title_bar"] 
                << table 
                    << besides [ td << anchor ! [href siteUrl] 
                                        << siteName  
                               , td << t
                               ]

askSect1Bar :: STXT.Sect1 -> Reader Env Html
askSect1Bar (STXT.Sect1 selected t _ _) = do
    s1s <- askSect1s 
    return $ thediv ! [identifier "sect1_bar"] 
                << table 
                    << besides [ (label (n==selected)) << sect1Anchor s1 
                               | s1@(STXT.Sect1 n _ _ _) <- s1s
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


sect1Anchor :: STXT.Sect1 -> Html
sect1Anchor s1@(STXT.Sect1 n t _ _) = 
    anchor ! [href (sect1Path s1)] << t

sect2Anchor :: STXT.Sect2 -> Html
sect2Anchor s2@(STXT.Sect2 _ t _) = 
    anchor ! [href (sect2Path s2)] << t


sect1Path :: STXT.Sect1 -> String
sect1Path s1@(STXT.Sect1 n t _ _) = show n ++ ".html"

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
    case (getDoc env) of
       doc@(STXT.Doc _ _ _) -> return $ doc
       (STXT.Error msg)   -> error $ "Error happen:\n" ++ msg
        
askDocTitle :: Reader Env String
askDocTitle = do 
    (STXT.Doc t _ _) <- askDoc
    return t

askSect1s :: Reader Env STXT.Sect1s
askSect1s = do 
    (STXT.Doc _ _ s1s) <- askDoc
    return s1s

askSect2s :: STXT.Sect1 -> Reader Env STXT.Sect2s
askSect2s (STXT.Sect1 _ _ _ s2s) = do 
    return s2s

elem2Html :: STXT.Elem -> Html
elem2Html (STXT.Para ps) = thediv ! [theclass "para"]
                                << paragraph << [paraObj2Html p | p <- ps]
elem2Html (STXT.Code c) = pre << c

paraObj2Html :: STXT.ParaObj -> Html
paraObj2Html (STXT.Str s) = toHtml s
paraObj2Html (STXT.Link t url) = anchor ! [href url] << t  


myMeta = meta ! [ httpequiv "Content-Type"
                , content   "text/html; charset=utf-8"
                ] 

myLink = thelink ! [ rel  "stylesheet"
                   , href "web.css" 
                   ] << noHtml

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
           
label :: Bool -> Html -> Html
label isSelected child = (if isSelected then
        td ! [theclass "selected"]   
    else 
        td) << child


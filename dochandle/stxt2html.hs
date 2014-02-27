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
webdir = "d:\\fhopehltb\\"
webcss = "d:\\stxt\\dochandle\\web.css"
index  = FP.combine webdir "index.html"

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
    mapM_ writeHtml (evalState getHtmls (Page { pSource  = src
                                              , pOutDir  = outDir
                                              , pDoc     = STXT.run c
                                              , pTitle   = ""
                                              , pContent = []
                                              , pSect1s  = []
                                              , pSect1   = Nothing
                                              , pSect2s  = []
                                              , pSect2   = Nothing
                                              }))
    --copyFile index "d:\\stxt\\fhopecc\\www\\index.html"
    --

writeHtml :: (FilePath, Html) -> IO ()
writeHtml (path, html) = do
    f <- openFile path WriteMode
    hSetEncoding f utf8
    hPutStr f (renderHtml html)
    putStrLn path
    hFlush f
    hClose f


getHtmls :: State Page [(FilePath, Html)]
getHtmls = do
    ihtml <- getIndexHtml
    s1htmls <- getSect1Htmls  
    s2htmls <- getSect2Htmls
    return $ ihtml : s1htmls ++ s2htmls

getIndexHtml :: State Page (FilePath, Html)
getIndexHtml = do
    p@(Page {pDoc = (STXT.Doc t cs s1s)})  <- get
    put p { pTitle   = t
          , pContent = cs
          , pSect1   = Nothing
          , pSect2   = Nothing
          , pSect1s  = s1s
          , pSect2s  = []
          }
    getPageHtml

getSect1Htmls :: State Page [(FilePath, Html)]
getSect1Htmls = do
    p@(Page {pSect1s=s1s}) <- get
    forM s1s $ \s1@(STXT.Sect1 n1 t cs s2s) -> do 
        put p{ pContent = cs
             , pSect1   = Just s1
             , pSect2s  = s2s
             , pSect2   = Nothing
             }
        getPageHtml

getSect2Htmls :: State Page [(FilePath, Html)]
getSect2Htmls = do
    p@(Page {pSect1s=s1s}) <- get
    s2s <- forM s1s $ \s1@(STXT.Sect1 _ _ _ s2s) -> do 
        forM s2s $ \s2@(STXT.Sect2 n t cs) -> do
            put p{ pContent = cs
                 , pSect1 = Just s1
                 , pSect2s = s2s
                 , pSect2 = Just s2
                 }
            getPageHtml
    return $ concat s2s

getIndexFile :: State Page String
getIndexFile = do
    Page {pOutDir = outdir} <- get
    return $ outdir ++ "\\" ++ "index.html"

getFilePath :: State Page String
getFilePath = do
    Page { pOutDir = outdir
         , pDoc    = doc
         , pSect1  = ss1
         , pSect2  = ss2
         } <- get

    let f = if isJust ss2 then
                sect2Path $ fromJust ss2
            else if isJust ss1 then
                    sect1Path $ fromJust ss1
                 else "index.html"
    return $ outdir ++ "\\" ++ f

getTitleBar = do
    Page{pTitle = title} <- get
    return $ thediv ! [identifier "title_bar"] 
                << table 
                    << besides [ td << anchor ! [href siteUrl] 
                                        << siteName  
                               , td << anchor ! [href "index.html"] 
                                        << title
                               ]

getSect1Bar :: State Page Html
getSect1Bar = do
    Page { pSect1s = s1s
         , pSect1  = ss1
         } <- get
    return $ if null s1s then toHtml ""
             else thediv ! [identifier "sect1_bar"] << table 
                      << if isJust ss1 then 
                            let (STXT.Sect1 selected _ _ _) = fromJust ss1
                            in besides [(label (n==selected)) << sect1Anchor s1 
                                       | s1@(STXT.Sect1 n _ _ _) <- s1s
                                       ]
                         else
                            besides [(label False) << sect1Anchor s1 
                                     | s1 <- s1s
                                     ]
 

getSect2Bar :: State Page Html
getSect2Bar = do
    Page { pSect2s = s2s 
         , pSect2  = ss2
         } <- get
    return $ if null s2s then toHtml ""
             else thediv ! [identifier "sect2_bar"] << table
                << if isJust ss2 then 
                      let selected = fromJust ss2
                      in aboves [ (label (s2 == selected )) << sect2Anchor s2 
                                |  s2 <- s2s
                                ]
                      else
                         aboves [(label False) << sect2Anchor s2
                                | s2 <- s2s
                                ]


getPageHtml :: State Page (FilePath, Html)
getPageHtml = do
    Page { pTitle = title
         , pContent = content
         } <- get
    titlebar <- getTitleBar
    sect1bar <- getSect1Bar 
    sect2bar <- getSect2Bar
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
    f <- getFilePath
    return (f, html)

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


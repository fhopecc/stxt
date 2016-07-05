import qualified STXT
import Text.Html
import Control.Monad.State
import System.Environment 
import System.IO
import qualified System.FilePath.Windows as FP
import Data.List
import Data.Maybe
import System.Console.GetOpt
import System.Directory
    
data Page = Page 
  { pSiteURL  :: String
  , pSiteName :: String
  , pBase     :: FilePath
  , pCSSPath  :: FilePath
  , pSource   :: FilePath
  , pOutDir   :: FilePath
  , pDoc      :: Maybe STXT.Doc
  , pTitle    :: String
  , pContent  :: STXT.Content
  , pSect1s   :: STXT.Sect1s
  , pSect1    :: Maybe STXT.Sect1
  , pSect2s   :: STXT.Sect2s
  , pSect2    :: Maybe STXT.Sect2
  , pSect3s   :: STXT.Sect3s
  } 

defaultPage = Page 
  { pSiteURL  = "http://fhopehltb.appspot.com" 
  , pSiteName = "剛的網站"
  , pBase     = "d:\\fhopehltb\\www"
  , pCSSPath  = "d:\\stxt\\dochandle\\web.css"
  , pSource   = ""
  , pOutDir   = ""
  , pDoc      = Nothing   
  , pTitle    = ""   
  , pContent  = []  
  , pSect1s   = [] 
  , pSect1    = Nothing 
  , pSect2s   = []     
  , pSect2    = Nothing
  , pSect3s   = []     
  }

options :: [OptDescr (Page -> IO Page)]
options = []

main = do
    args <- getArgs
 
    let (actions, nonOptions, errors) = getOpt RequireOrder options args
    
    when (null nonOptions) 
        (error "syntax: stxt2html options source")

    let src = head nonOptions
    let srcdir = FP.takeDirectory src
    let basename = FP.takeBaseName src
    let basedir = pBase defaultPage
    f <- openFile src ReadMode
    hSetEncoding f utf8
    c <- hGetContents f
    doc <- STXT.runInclude srcdir c

    page <- foldl (>>=) 
                  ( return defaultPage 
                            { pSource = src  
                            , pDoc    = Just doc
                            , pOutDir = FP.combine basedir basename
                            }
                  ) actions 

    mapM_ writeHtml (evalState getHtmls page)
    
writeHtml :: (FilePath, Html) -> IO ()
writeHtml (path, html) = do
    createDirectoryIfMissing True $ FP.takeDirectory path 
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
    p@(Page {pDoc = Just (STXT.Doc t cs s1s)})  <- get
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
        forM s2s $ \s2@(STXT.Sect2 n t cs s3s) -> do
            put p{ pContent = cs
                 , pSect1 = Just s1
                 , pSect2s = s2s
                 , pSect2 = Just s2
                 , pSect3s = s3s
                 }
            getPageHtml
    return $ concat s2s

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
    Page 
      { pTitle    = title
      , pSiteName = siteName
      , pSiteURL  = siteUrl
      } <- get
    return $ thediv ! [identifier "title_bar"] << table 
               << besides [ td << anchor ! [href siteUrl] << siteName  
                          , td << anchor ! [href "index.html"] << title
                          ]

getSect1Bar :: State Page Html
getSect1Bar = do
    Page { pSect1s = s1s
         , pSect1  = ss1
         } <- get
    let selected = fromMaybe STXT.EmptySect1 ss1
    return $ if null s1s then noHtml
             else thediv ! [identifier "sect1_bar"] << table 
                    << besides [(label (s1 == selected)) << sect1Anchor s1 
                               | s1 <- s1s
                               ]

getSect2Bar :: State Page Html
getSect2Bar = do
    Page { pSect2s = s2s 
         , pSect2  = ss2
         } <- get
    let selected = fromMaybe STXT.EmptySect2 ss2
    return $ if null s2s then noHtml
             else thediv ! [identifier "sect2_bar"] << table
                    << aboves [(label (s2 == selected)) << sect2Anchor s2 
                              | s2 <- s2s
                              ]

getPageHtml :: State Page (FilePath, Html)
getPageHtml = do
    Page { pTitle   = title
         , pContent = content
         , pSect2s  = sect2s
         , pDoc     = Just doc
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
                        << map (elem2Html doc) content 
                       +++ map (sect2ToHtml doc) sect2s
    f <- getFilePath
    return (f, html)

sect1Anchor s1@(STXT.Sect1 n t _ _) = 
    anchor ! [href (sect1Path s1)] << t

sect2Anchor s2@(STXT.Sect2 _ t _ _) = 
    anchor ! [href (sect2Path s2)] << t

sect1Path (STXT.Sect1 n _ _ _) = show n ++ ".html"

sect2Path (STXT.Sect2 (n1, n2) _ _ _) = 
    show n1 ++ "_" ++ show n2 ++ ".html"

sect2ToHtml :: STXT.Doc -> STXT.Sect2 -> Html
sect2ToHtml doc (STXT.Sect2 (n1, n2) title content s3s) = 
    thediv ! [theclass "sect3"]
        << h1 << title
       +++ map (elem2Html doc) content 
       +++ map (sect32Html doc) s3s 

sect32Html :: STXT.Doc -> STXT.Sect3 -> Html
sect32Html doc (STXT.Sect3 _ title content) = 
    thediv ! [theclass "sect3"]
        << h2 << title
       +++ map (elem2Html doc) content 

elem2Html :: STXT.Doc -> STXT.Elem -> Html
elem2Html doc (STXT.Para ps) = 
    thediv ! [theclass "para"]
       << paragraph << [paraObj2Html doc p | p <- ps]

elem2Html doc (STXT.Code c) = pre << c

paraObj2Html :: STXT.Doc -> STXT.ParaObj -> Html
paraObj2Html doc (STXT.Str s) = toHtml s
paraObj2Html doc (STXT.Link t url) = anchor ! [href url] << t  
paraObj2Html doc (STXT.ILink t) = 
    let s1 = doc `STXT.getSect1` t
    in if not (isNothing s1) then
          sect1Anchor $ fromJust s1 
       else
          let s2 = doc `STXT.getSect2` t 
          in if not (isNothing s2) then
                sect2Anchor $ fromJust s2
             else
                toHtml t

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

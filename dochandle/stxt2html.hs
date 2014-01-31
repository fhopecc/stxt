import qualified STXT
import Text.Html
import Control.Monad.Reader
import System.IO

main = do 
    let src = "d:\\stxt\\doc\\vbscript\\vbscript.txt"
    f <- openFile src ReadMode
    hSetEncoding f utf8
    c <- hGetContents f
    
    mapM_ writeHtml (runReader askHtmls (Env { getSource = src
                                       , getOutDir = "d:\\stxt\\myweb\\vbscript"
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
    return $ ihtml : s1htmls

askIndexHtml :: Reader Env (FilePath, Html)
askIndexHtml = do
    f <- askIndexPath
    html <- askIndexPage
    return (f, html)

askSect1Htmls :: Reader Env [(FilePath, Html)]
askSect1Htmls = do
    s1s <- askSect1s
    mapM askSect1Html s1s

askSect1Html :: STXT.Sect1 -> Reader Env (FilePath, Html)
askSect1Html s1@(STXT.Sect1 n t s2s) = do
    doc@(STXT.Doc _ s1s) <- askDoc
    sect1bar <- askSect1Bar s1
    sect2bar <- askSect2Bar (head s1s)
    let html = thehtml ! [lang "zh-tw"] 
                << header 
                    << myMeta
                   +++ myLink
                   +++ thetitle << t
               +++ body 
                    << sect1bar 
                   +++ sect2bar 
    s1File <- askSect1File s1
    return $ (s1File, html)


data Env = Env { getSource :: FilePath  
               , getOutDir :: FilePath  
               , getDoc    :: STXT.Doc
               }

askIndexPage :: Reader Env Html
askIndexPage = do
    t <- askDocTitle
    doc@(STXT.Doc _ s1s) <- askDoc
    sect1bar <- askSect1Bar $ head s1s
    sect2bar <- askSect2Bar (head s1s)
    return $ thehtml ! [lang "zh-tw"] 
                << header 
                    << myMeta
                   +++ myLink
                   +++ thetitle << t
               +++ body 
                    << sect1bar 
                   +++ sect2bar 


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
    return $ outdir ++ "\\" ++ show n1 ++ show n2 ++ ".html" 

askSect1Bar :: STXT.Sect1 -> Reader Env Html
askSect1Bar (STXT.Sect1 selected t _) = do
    s1s <- askSect1s 
    return $ thediv ! [identifier "sect1_bar"] 
                << table 
                    << besides [ (label (n==selected)) << sect1Anchor s1 
                               | s1@(STXT.Sect1 n _ _) <- s1s
                               ]

askSect2Bar :: STXT.Sect1 -> Reader Env Html
askSect2Bar s1 = do
    s2s <- askSect2s s1 
    return $ thediv ! [identifier "sect2_bar"] 
                << table 
                    << aboves [ (label (1==n2)) << sect2Anchor s2 
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

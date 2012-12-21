import Control.Monad
import Control.Exception as X
import System.IO
import System.Directory
import System.Exit
import Text.Printf
import Text.HTML.TagSoup
import Text.XML.HaXml.Html.Parse
import Text.Regex.Posix
import Text.Regex
import CP950
import Data.Char
import Data.List


main = do
   travel "d:\\planet" printFileContent
      
printFileContent f = do
    h <- openFile f ReadMode 
    putStrLn f
    hSetEncoding h  cp950
    hSetEncoding stdout cp950
    c <- hGetContents h
    let d = htmlParse f c
    print d 
    --X.catch (putStrLn c) printErr
    hClose h
    exitSuccess
    where
        printErr :: SomeException -> IO ()
        printErr e = do
            putStrLn $ show(e) ++ "failed"
            h2 <- openFile f ReadMode 
            hSetEncoding h2 utf8 
            c2 <- hGetContents h2
            putStrLn c2
            print "try again OK"
            hClose h2

parsePair = do 
    hSetEncoding stdout cp950

    h <- openFile "plant.txt" ReadMode
    hSetEncoding h utf8
    c <- hGetContents h

    o <- openFile "plant_pairs.txt" WriteMode
    hSetEncoding o utf8
    
    let ls = lines c
    forM_ ls $ \l -> do
        let w = l =~ "‧[^‧]+" :: [[String]]
        let ol = unlines.map unwords $ w 
        putStrLn ol 
        hPutStrLn o ol 
    
printIsCJK :: Char -> IO ()
printIsCJK c 
    | isCJK c   = putStrLn $ c : " 是中文" 
    | otherwise = putStrLn $ c : " 不是中文" 
                    
isCJK :: Char -> Bool
isCJK c = c >= '\x4E00' && c <= '\x9FFF'

findFiles = do
    fs <- getDirectoryContents "d:\\planet"
    hSetEncoding stdout cp950
    putStr $ unlines fs

travel dir handler = do
    fs' <- getDirectoryContents dir
    let fs = delete "." . delete ".." $ fs'
    forM_ fs $ \f -> do
       let path = dir ++ "\\" ++ f
       when (isSuffixOf ".htm" path) $ handler path
       isDir <- doesDirectoryExist path 
       when isDir $ travel path handler

extractText f = do 
    h <- openFile f ReadMode 
    wh <- openFile "plant.txt" AppendMode
    putStrLn f
    hSetEncoding h  cp950
    hSetEncoding wh cp950
    c <- hGetContents h
    let ts = parseTags c
    let o = (filter (not . isSpace) . unlines . map fromTagText . filter isTagText) ts
    X.catch (hPutStrLn wh o) printErr
    hClose h
    hClose wh
    --exitSuccess
    where
        printErr :: SomeException -> IO ()
        printErr e = do
            putStrLn $ show(e) ++ "failed"
            h2 <- openFile f ReadMode 
            wh2 <- openFile "plant2.txt" AppendMode
            hSetEncoding h2 utf8 
            hSetEncoding wh2 utf8 
            c2 <- hGetContents h2
            let ts2 = parseTags c2
            let o2 = (filter (not . isSpace) . unlines . map fromTagText . filter isTagText) ts2
            hPutStrLn wh2 o2
            print "try again OK"
            hClose h2
            hClose wh2
            return ()

printFileName f = do
    let lastSepInd = last . findIndices (== '\\')                    $ f 
    let fn         = takeWhile (/= '.').snd.splitAt (lastSepInd + 1) $ f
    when ('%' `elem` fn) $ do
        putStrLn $ ".............." ++ fn ++ ".............." 
        h <- openFile f ReadMode 
        hSetEncoding h cp950
        c <- hGetContents h
        hClose h

--parsePair :: String -> [(String, Stirng)]
--parsePair src = 
 --   src =~ "‧[^.]*" :: [String]
test = "龜甲龍龜甲龍龜甲龍01龜甲龍00龜甲龍02龜甲龍03龜甲龍-球莖05龜甲龍05龜甲龍06龜甲龍-球莖00龜甲龍-球莖01龜甲龍-球莖02龜甲龍-球莖03龜甲龍-球莖04龜甲龍04龜甲龍-球莖06龜甲龍-球莖07龜甲龍-葉0龜甲龍-葉1龜甲龍-葉2龜甲龍-葉3龜甲龍-葉4龜甲龍-葉5‧中文名稱：龜甲龍‧英文名稱：TurtleBack，Elephant'sFoot，Hottentots'Bread‧學名：DioscoreaelephantipesDioscoreaelephantipes(L'Her.)Engl.‧科名：薯蕷科(Dioscoreaceae)薯蕷屬(Dioscorea)‧別名：‧原產地：南非開普敦省。‧分佈：栽培‧用途：觀賞用‧莖：莖綠色，蔓性，長約1~2公尺。‧葉：葉心形三角狀，長約6~7公分。‧花：花朵為雌雄異株，花朵細小，10~15朵成串開放，會發出如糖果般的淡淡香味，但雌株數量比雄株少。‧果實：果實蒴果。‧特性：莖幹狀多肉植物，塊根淺褐色，幼苗時呈球型，成株後表皮有龜裂，形成許多獨立小塊，如石頭堆疊狀，宛如龜甲，其名便由此而來。莖綠色，蔓性，長約1~2公尺。葉心形三角狀，長約6~7公分。花朵為雌雄異株，花朵細小，10~15朵成串開放，會發出如糖果般的淡淡香味，但雌株數量比雄株少。果實蒴果。‧更多資料：http://web.igarden.com.tw/magazine/show_one.php?serial_s=1191&serial_m=46龜甲龍http://www.plantzafrica.com/plantcd/dioscoreleph.htm‧拍攝地點：新社。(970313~971107)var_gaq=_gaq||[];_gaq.push(['_setAccount','UA-27361255-1']);_gaq.push(['_trackPageview']);(function(){varga=document.createElement('script');ga.type='text/javascript';ga.async=true;ga.src=('https:'==document.location.protocol?'https://ssl':'http://www')+'.google-analytics.com/ga.js';vars=document.getElementsByTagName('script')[0];s.parentNode.insertBefore(ga,s);})();"



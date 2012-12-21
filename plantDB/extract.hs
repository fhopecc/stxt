import Control.Monad hiding (join) 
import Control.Exception 
import System.IO
import System.Directory
import System.Exit
import System.FilePath.Posix
import Text.Printf
import Text.XML.HXT.Core hiding (utf8, when)
import Text.Regex.Posix hiding (extract)
import Text.Regex
import GHC.IO.Encoding.CP950
import Data.Char
import Data.List
import Prelude hiding (catch)
import Data.String.Utils
import DetectEncoding

main = do
   hSetEncoding stdout cp950
   travel "d:\\planet" printFileContent
      
printFileEncode f = do
    encode <- detectFileEncoding f
    when (encode == UTF8) $
        putStrLn (f ++ " -> " ++ show encode)

printFileContent f = do
    putStrLn f

    h <- openFile f ReadMode 
    encode <- detectFileEncoding f
    case encode of 
        UTF8  -> hSetEncoding h utf8
        CP950 -> hSetEncoding h cp950

    c <- hGetContents h
    --putStrLn c
    r <- extractName c
    putStr $ join "][" r
    hClose h

extract html = do
  let doc = readString [withParseHTML yes, withWarnings no] html
  links <- runX $ doc >>> multi getText >. concat
  return $ join "][" links

extractName html = do
  names <- runX $ readString [withParseHTML yes, withWarnings no] html
       >>> ( getField "中文名稱"
         <+> getField "英文名稱"
         <+> getField "學名"
         <+> getField "科名"
         <+> getField "別名"
         <+> getField "原產地"
         <+> getField "分布"
         <+> getField "用途"
         <+> getField "莖："
         <+> getField "葉："
         <+> getField "花："
         <+> getField "果實："
         <+> getField "特性："
           )
  return $ map (delete ' ') names


significant :: String -> Bool
significant = not . all (`elem` "\n\r\t")

getField str = multi (hasName "tr")
           >>> removeAllWhiteSpace
           >>> hasInfix str
               `guards` 
               (   deep getText 
               >>> isA significant >. concat)


hasInfix str = 
             ( deep getText 
           >>> isA significant 
            >. concat 
           >>> isA (isInfixOf str)
             )    


shead l = if null l then [] else head l
slast l = if null l then [] else last l

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
    fs <- getDirectoryContents "d:/planet"
    hSetEncoding stdout cp950
    putStr $ unlines fs

travel dir handler = do
    fs' <- getDirectoryContents dir
    let fs = fs' \\ [".", ".."]
    forM_ fs $ \f -> do
       let path = dir </> f
       when (isSuffixOf ".htm" path) $ handler path
       isDir <- doesDirectoryExist path 
       when isDir $ travel path handler

printFileName f = putStrLn f

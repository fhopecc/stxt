import Control.Monad
import System.Directory
import System.Environment
import System.Exit
import System.FilePath.Windows
import System.IO
import qualified Data.ByteString.Char8 as B
import Data.List
import Text.Regex
import Text.Regex.Posix
import GHC.IO.Encoding.CP950
import Data.ByteString.CP950

main = do
    args <- getArgs
    when (length args < 2) $ do
        putStrLn "xrename pattern replacement"
        exitFailure

    let p = "(.*)" ++ (decodeCP950 (B.pack (args !! 0))) ++ "(.*)"
    let r = args !! 1

    hSetEncoding stdout cp950
    d <- getCurrentDirectory
    travel d (filterF p r)

printFileName f = do
    putStrLn f

filterF p r f = do 
    --isDir <- doesDirectoryExist p
    --when (f =~ p && not isDir) $ do
    let fn = takeFileName f
    when (fn =~ p) $ do
        -- subRegex bug 
        --let nfn = subRegex (mkRegex p) fn ("\\1" ++ r ++ "\\2")
        
        let nfn = subRegex (mkRegex p) fn ("\\1" ++ (take (length r - 1) r) 
                                                 ++ "\\2")
        --renameFile f nfn
        putStrLn $ "rename \"" ++ f ++ "\" \"" ++ nfn ++ "\""

travel dir handler = do
    fs' <- getDirectoryContents dir
    let fs = fs' \\ [".", ".."]
    forM_ fs $ \f -> do
       let path = dir </> f
       handler path
       isDir <- doesDirectoryExist path 
       when isDir $ travel path handler

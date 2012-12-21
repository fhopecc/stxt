import System.Directory
import System.IO
import Control.Monad
import Data.List
import GHC.IO.Encoding.CP950 

sys = ["QIH","EIP", "EGX","LKM","LWS","QIA","QID"
      ,"QIN","QIW",	"QIY","YMQ"	
      ]
main = do
    hSetEncoding stdout cp950
    fs' <- getDirectoryContents "d:\\hltb\\tmp\\"
    let fs = fs' \\ [".", ".."]
    forM_ fs $ \f -> do
        let base = "d:\\hltb\\tmp\\"
        let nf   = base ++ "花蓮縣系統測試報告V1_0(" 
                      ++ getSys f ++ ")審查意見.pdf"
        let p    = base ++ f
        renameFile p nf
    where 
        getSys f = 
            let i = (read $ take 3 $ drop 25 f) :: Int
            in sys !! (i + 1)
                 

import Control.Monad
import Char
import IO
import List
import Network.FTP.Client
import System
import System.Console.GetOpt
import System.IO.Binary

    
main = do
    args <- getArgs
 
    -- Parse options, getting a list of option actions
    let (actions, nonOptions, errors) = getOpt RequireOrder options args
 
    -- Here we thread startOptions through all supplied option actions
    opts <- foldl (>>=) (return startOptions) actions
 
    let Options { optResend = resend
                , optPattern = pattern } = opts

    print pattern

list_file = do
    r <- easyConnectFTP "10.66.251.1"
    --r <- connectFTP "localhost" 21
    h <- return (fst r)
    print (isPassive h)
    hp <- return (setPassive h False)
    print (isPassive hp)
    login hp "web_export" (Just "trop_111") Nothing
    c <- dir hp Nothing
    print c
    --putbinary h "syslog2.bin" "c:\\syslog.bin"
    quit hp
    --r2 <- getbinary h "syslog.log"
    --writeBinaryFile "c:\\syslog.bin" (fst r2)

 
data Options = Options  { optResend  :: Bool
                        , optPattern :: String
                        }

startOptions :: Options
startOptions = Options  { optResend  = False
                        , optPattern = ""
                        }

options :: [ OptDescr (Options -> IO Options) ]
options =
    [ Option "r" ["resend"]
        (NoArg
            (\opt -> return opt { optResend = True }))
        "Enable verbose messages"
 
    , Option "p" ["pattern"]
        (ReqArg
            (\arg opt -> return opt { optPattern = arg })
            "PATTERN")
        "Pattern String"

    , Option "V" ["version"]
        (NoArg
            (\_ -> do
                hPutStrLn stderr "Version 0.01"
                exitWith ExitSuccess))
        "Print version"
 
    , Option "h" ["help"]
        (NoArg
            (\_ -> do
    	        prg <- getProgName
                hPutStrLn stderr (usageInfo prg options)
                exitWith ExitSuccess))
        "Show help"
    ]

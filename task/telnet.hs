module Main where
import Control.Concurrent
import Data.Char
import Data.Time
import Data.Time.Format
import Network
import Network.Socket
import System.IO
import System.Locale
import System(getArgs)
import Control.Monad
import Text.Printf

connectTelnet :: String -> String -> IO Handle
connectTelnet ip port = do
    args <- getArgs      
    addrinfos <- getAddrInfo Nothing (Just (ip)) (Just port)
    let serveraddr = head addrinfos
    sock <- socket (addrFamily serveraddr) Stream defaultProtocol       
    connect sock (addrAddress serveraddr)       
    h <- socketToHandle sock ReadWriteMode       
    hSetBuffering h (BlockBuffering Nothing)       
    return h

backupCisco2623 :: IO ()
backupCisco2623 = withSocketsDo $ do       
    h <- connectTelnet "10.66.254.254" "23"
    expect h "Username:"
    hPutStrLn h "ce"
    hFlush h
    expect h "Password:"
    hPutStrLn h "uecicsed"
    hFlush h
    expect h "#"
    now <- getCurrentTime
    let strDay = formatTime defaultTimeLocale "%Y%m%d" now
    let dest = "tftp://10.66.4.56/c2623_" ++ strDay ++ ".cfg"
    hPutStrLn h ("copy running-config " ++ dest ++ "\n\n")       
    hFlush h       
    expect h "#"
    print "OK"

backupEnterasysC2 :: IO ()
backupEnterasysC2 = withSocketsDo $ do       
    h <- connectTelnet "10.66.4.254" "23"
    expect h "Username:"
    hPutStrLn h "ce"
    hFlush h
    expect h "Password:"
    hPutStrLn h "uecicsed"
    hFlush h
    expect h "->"
    hPutStrLn h "delete configs/EterasysC2.cfg"
    hFlush h
    expect h "->"
    hPutStrLn h "show config outfile configs/EterasysC2.cfg"
    hFlush h
    expect h "->"
    now <- getCurrentTime
    let strDay = formatTime defaultTimeLocale "%Y%m%d" now
    let dest = "tftp://10.66.4.56/EnterasysC2_" ++ strDay ++ ".cfg"
    hPutStrLn h $ "copy configs/EterasysC2.cfg " ++ dest
    hFlush h
    expect h "->"
    print "OK"
 
backupFG100A :: IO ()
backupFG100A = withSocketsDo $ do       
    h <- connectTelnet "192.168.1.254" "23"
    expect h [cIAC, cDO, oNEWENVIRON]

    hPutStr h [cIAC, cWILL, oTerminalType, cIAC, cWILL, oNAW]
    hFlush h
    print "will Termial type"

    expect h [cIAC, cDO, oNAW]

    hPutStr h [cIAC, cWONT, oTerminalSpeed, cIAC, cWONT, oXDISPLOC, 
               cIAC, cWILL, oNEWENVIRON]
    hFlush h
    print "will NEWENV"
    
    hPutStr h [cIAC, cSB, oNAW, '\0', '\80', '\0', '\25', cIAC, cSE]
    hFlush h
    print "will window h 80 w 25"

    expect h [cIAC, cSB, oTerminalType, '\01', cIAC, cSE]

    hPutStr h [cIAC, cSB, oNEWENVIRON, '\0', cIAC, cSE,
            cIAC, cSB, oTerminalType, '\0', 'A', 'N', 'S', 'I', cIAC, cSE]
    hFlush h
    print "terminal type ansi"

    expect h [cIAC, cDO, oRemoteFlowControl]
    hPutStr h [cIAC, cDO, oSupressGoAHEAD, 
               cIAC, cWILL, oEcho, 
               cIAC, cDONT, oStatus, 
               cIAC, cWONT, oRemoteFlowControl]
    hFlush h
    print "echo"

    expect h "login:"
    hPutStr h "admin\r\n"
    hFlush h
    expect h "Password:"
    hPutStr h "!tsinim9\r\n"
    hFlush h
    expect h " #"
    now <- getCurrentTime
    let strDay = formatTime defaultTimeLocale "%Y%m%d" now
    let dest = "FG100A_" ++ strDay ++ ".cfg"
    hPutStrLn h $ "execute backup full-config tftp " ++ dest ++ " 192.168.1.101"
    hFlush h
    expect h " #"
    print "OK"

printName cmd = 
    case lookup  (ord cmd) mapCmdName of 
    Just x -> do 
        printf "%X  " $ ord cmd
        printf "%d  " $ ord cmd
        print  x
    Nothing -> do
        --print $ show cmd
        printf "%X  " $ ord cmd
        printf "%d\n" $ ord cmd


expect :: Handle -> String -> IO ()
expect h p =
    let loop i = 
            if i >= length p then return ()
            else do
                let c = p !! i
                printName c
                c' <- hGetChar h      
                printName c'
                if c == c' then loop (i + 1)
                else loop 0
    in loop 0 
            
oEcho = '\1'
oSupressGoAHEAD = '\3'
oStatus = '\5'
oRemoteFlowControl = '\33'
oTerminalType = '\24'
oNAW = '\31'
oTerminalSpeed = '\32'
oXDISPLOC = '\35'
oNEWENVIRON = '\39'

cSE = '\240'
cSB = '\250'
cWILL =     '\251'
cWONT =     '\252'
cDO =     '\253'
cDONT =   '\254'
cIAC =    '\255'

mapCmdName = [
    (1, "ECHO"),
    (3, "Supress go ahead"),
    (24, "Termial Type"),
    (31, "NAW"),
    (32, "TerminalSpeed"),
    (35, "XDISPLOC"),
    (39, "NEWENVIRON"),
    (240, "SE"),
    (250, "SB"),
    (251, "WILL"),
    (252, "WONT"),
    (253, "DO"),
    (254, "DONT"),
    (255, "IAC")
    ]

main = backupFG100A

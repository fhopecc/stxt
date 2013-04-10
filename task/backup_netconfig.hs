module Main where
import Control.Concurrent
import Control.Exception
import Control.Monad
import Codec.Compression.GZip
import Data.Char
import Data.List
import Data.Time
import Data.Time.Format
import Data.String.Utils
import qualified Data.ByteString.Lazy as B
import Network
import Network.Socket
import System
import System.IO
import System.Cmd
import System.Exit
import System.Locale
import System.Timeout
import System.Directory
import GHC.IO.Encoding.CP950
import Text.Printf

outerONMSIP = "192.168.1.13"
innerONMSIP = "10.66.4.17"

--main = backupONMSDB outerONMSIP
main = cmd

cmd = do
    hSetEncoding stdout cp950
    args <- getArgs
    let source = map toUpper. dropWhile (== ' ') . head $ args 

    m <- newEmptyMVar
    forkIO $ case source of 
         "FG100A"  -> do backupFG100A 
                         putMVar m "backupFG100A OK"
         "FG200B"  -> do backupFG200B 
                         putMVar m "backupFG200B OK"
         "C2621"   -> do backupC2621 
                         putMVar m "backupC2621 OK"
         "EK6"     -> do backupEK6 
                         putMVar m "backupEK6 OK"
         "INNER"   -> do backupFG200B
                         putStrLn "備份 FG200B 成功!"
                         backupC2621
                         putStrLn "備份 C2621 成功!"
                         backupEK6 
                         putStrLn "備份 EK6 成功!"
                         backupONMSDB innerONMSIP
            
                         putStrLn "備份 ONMSDB 成功!"
                         putMVar m "備份內網設備成功!"
         "OONMS"   -> do backupONMSDB outerONMSIP
                         putMVar m "備份外網ONMS成功!"
         "IONMS"   -> do backupONMSDB innerONMSIP
                         putMVar m "備份內網ONMS成功!"
         otherwise -> do showHelp
                         putMVar m ""
        
    forkIO $ forever $ do
        c <- getChar
        when (c == 'q') $ putMVar m "User interrupt program!"

    r <- takeMVar m

    putStrLn r 

showHelp = do
    putStrLn "backup_netconfig SOURCE" 
    putStrLn "SOUCE = [FG100A|FG200B|C2621|EK6|INNER|OUTER]" 
    putStrLn "Version 0.4" 

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

backupC2621 = withSocketsDo $ do       
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
    let dest = "tftp://10.66.4.56/c2621_" ++ strDay ++ ".cfg"
    hPutStrLn h ("copy running-config " ++ dest ++ "\n\n")       
    hFlush h       
    expect h "#"
    print "OK"

backupEK6 :: IO ()
backupEK6 = withSocketsDo $ do       
    h <- connectTelnet "10.66.4.254" "23"
    expect h "Username:"
    hPutStrLn h "admin"
    hFlush h
    expect h "Password:"
    hPutStrLn h ""
    hFlush h
    expect h "->"
    hPutStrLn h "delete slot7/EK6.cfg"
    hFlush h
    expect h "->"
    hPutStrLn h "show config outfile slot7/EK6.cfg"
    hFlush h
    expect h "->"
    now <- getCurrentTime
    let strDay = formatTime defaultTimeLocale "%Y%m%d" now
    let dest = "tftp://10.66.4.56/EK6_" ++ strDay ++ ".cfg"
    hPutStrLn h $ "copy slot7/EK6.cfg " ++ dest
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

backupFG200B :: IO ()
backupFG200B = withSocketsDo $ do       
    h <- connectTelnet "10.66.253.201" "23"
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
    let dest = "FG200B_" ++ strDay ++ ".cfg"
    hPutStrLn h $ "execute backup full-config tftp " ++ dest ++ " 10.66.4.56"
    hFlush h
    expect h " #"

backupONMSDB host = do
    now <- getCurrentTime
    let strDay = formatTime defaultTimeLocale "%Y%m%d" now

    let pgDump  = "\"c://Program Files//PostgreSQL//9.0//bin//pg_dump.exe\""
    let dir     = "bak\\"
    let host'   = replace "." "_" host
    let dest    = printf "%sonmsdb_%s_%s" dir host' strDay
    let cmd = printf "%s -h %s -U opennms -f %s opennms" pgDump host (dest::String)
    exitCode <- system cmd
    unless (exitCode == ExitSuccess) (putStrLn $ host ++ 
                                                   "ONMSDB 備份失敗!")
    B.readFile dest >>= (B.writeFile $ dest ++ ".gz") . compress
    removeFile dest
    putStrLn $ host ++ "ONMSDB 備份成功!"

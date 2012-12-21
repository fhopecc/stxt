import Network (listenOn, withSocketsDo, accept, PortID(..), Socket)
import System (getArgs)
import System.IO 
import Control.Concurrent
import Control.Monad

main = withSocketsDo $ do
    args <- getArgs
    print args
    let port = fromIntegral (read $ head args :: Int)
    sock <- listenOn $ PortNumber port
    putStrLn $ "Listening on " ++ (head args)

    forkIO $ forever $ do
        (handle, _, _) <- accept sock
        hSetBuffering handle NoBuffering
        forkIO $ commandProcessor handle

    m <- newEmptyMVar
    forkIO $ forever $ do
        c <- getChar
        when (c == 'q') $ do 
             putMVar m "user enter q to exit"

    r <- takeMVar m
    print r 

commandProcessor :: Handle -> IO ()
commandProcessor handle = do
    line <- hGetLine handle
    if line == "\r" 
    then do print line
            hPutStrLn handle "read requst ok"
            hClose handle
    else do print line
            hPutStrLn handle $ "echo " ++ line
            commandProcessor handle

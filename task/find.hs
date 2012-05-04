module Main( main ) where

import System( getArgs )
import System.Console.GetOpt

main = do
  args <- getArgs
  case getOpt RequireOrder options args of
    (flags, [],      []) -> print $ length flags
    (_,     nonOpts, []) -> error $ "unrecognize" ++ unwords nonOpts
    (_,     _,       msgs) -> error $ concat msgs ++ usageInfo header options

data Flag = Version

options :: [OptDescr Flag]
options = [ Option ['V'] ["version"] (NoArg Version) "show version number"]
header = "Usage: main [OPTION...]"


module Main (module Main) where

import System.Console.GetOpt
import System
import Control.Monad
import IO
import List
import Char

data Opt = Opt
    { optInput		    :: IO String
    , optOutput		    :: String -> IO ()
    , optVerbose	    :: Bool
    , optFilter		    :: String -> String
    }

startOpt :: Opt
startOpt = Opt
    { optInput		    = exitErrorHelp "use -i option to set input"
			    -- a simple way to handle mandatory flags

    , optOutput		    = putStr
    , optVerbose	    = False
    , optFilter		    = id
    }

options :: [OptDescr (Opt -> IO Opt)]
options =
    [ Option "h" ["help"]
	(NoArg (\opt -> exitHelp)) 
	"Show usage info"

    , Option "i" ["input"]
	(ReqArg
	    (\arg opt -> do
		return opt { 
		    optInput =
			case arg of
			    "-" -> getContents
			    _	-> readFile arg 
		})
	    "FILE")
	"Input file, - for stdin"

    , Option "s" ["string"]
	(ReqArg
	    (\arg opt -> return opt { optInput = return arg })
	    "FILE")
	"Input string"

    , Option "n" ["newline"]
	(NoArg
	    (\opt -> return opt { optOutput = putStrLn }))
	"Add newline on output"

    , Option "v" ["verbose"]
	(NoArg
	    (\opt -> return opt { optVerbose = True }))
	"Be verbose"

    , Option "V" ["version"]
	(NoArg
	    (\_ -> do
		hPutStrLn stderr "0.01"
		exitWith ExitSuccess))
	"Print version"

    , Option "U" ["uppercase"]
	(NoArg (addFilter (map toUpper)))
	"Convert to uppercase"

    , Option "r" ["reverse"]
	(NoArg (addFilter reverse))
	"Reverse"

    , Option "t" ["tac"]
	(NoArg (addFilter (unlines . reverse . lines)))
	"Reverse lines"

    , Option "d" ["delete"]
	(ReqArg (\arg -> addFilter (filter (not . (`elem` arg)))) "CHARS")
	"Delete characters"

    , Option "" ["drop"]
	(ReqArg 
	    (\arg opt -> do
		n <- readArg "drop" arg
		addFilter (drop n) opt)
	    "NUM")
	"Drop n first characters"
    ]
  where
    -- helper for composing filters - without it would be too easy to forget
    -- something
    addFilter f opt = return opt { optFilter = f . optFilter opt }

main = do
    (opts, _) <- parseOptions

    let Opt { optVerbose = verbose
	    , optInput = input
	    , optOutput = output
	    , optFilter = filt } = opts

    when verbose (hPutStrLn stderr "I am verbose.")

    input >>= output . filt

showHelp :: IO ()
showHelp = do
    prg <- getProgName
    hPutStrLn stderr (usageInfo prg options)
    hFlush stderr

exitHelp :: IO a
exitHelp = do
    showHelp
    exitWith ExitSuccess

exitError :: String -> IO a
exitError msg = do
    hPutStrLn stderr msg
    hPutStrLn stderr ""
    exitFailure

exitErrorHelp :: String -> IO a
exitErrorHelp msg = do
    hPutStrLn stderr msg
    hPutStrLn stderr ""
    showHelp
    exitFailure

readArg :: Read a => String -> String -> IO a
readArg name arg = do
    case reads arg of
	((x, []) : _)	-> return x
	_		-> exitError $ "Can't parse " ++ name ++ " arg"

parseOptions :: IO (Opt, [String])
parseOptions = do
    (optsActions, rest, errors) <- getArgs >>= return . getOpt RequireOrder options

    when (not (null errors)) $ do
	mapM_ (hPutStrLn stderr) errors
	showHelp
	exitFailure

    opts <- foldl (>>=) (return startOpt) optsActions
    return (opts, rest)

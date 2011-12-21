module Main where

import Control.Monad ( when, unless )
import Control.Monad.Reader ( ReaderT(runReaderT) )
import System.Console.GetOpt
import System.Exit ( exitWith, ExitCode (..) )
import System.Environment ( getArgs, getProgName )
import System.IO ( hPutStrLn, stderr )
import Text.Bookbuilder -- TODO ( compile )
import Text.Bookbuilder.Config ( Options(..), configure, Config )
import Text.Bookbuilder.Location ( Location )

defaultOptions :: Options
defaultOptions = Options
	{ optRoot       = ""
	, optSourceDir  = "src"
	, optProfileDir = "profiles"
	, optBuildDir   = "build"
	, optStart      = Nothing
	, optEnd        = Nothing
    , optVars       = []
	, optDetect     = True
	, optDebug      = False
	, optHelp       = False }

-- | A list of functions, each transforming the options data structure
-- | in response to a command-line option.
options :: [OptDescr (Options -> IO Options)]
options =
	[ Option "s" ["src"]
		(ReqArg (\arg opt -> return opt { optSourceDir = arg }) "DIR")
		"directory containing the book source files"

	, Option "p" ["profiles"]
		(ReqArg (\arg opt -> return opt { optProfileDir = arg }) "DIR")
		"directory containing profiles to output"

	, Option "o" ["build"]
		(ReqArg (\arg opt -> return opt{ optBuildDir = arg }) "DIR")
		"directory in which to place compiled files"

	, Option "a" ["author"]
		(ReqArg (\arg opt -> do
            let vars = optVars opt
            return opt{ optVars = ("author", arg):vars }) "NAME")
		"the author template variable"

	, Option "d" ["debug"]
		(NoArg (\opt -> return opt{ optDebug = True }))
		"write the intermediary files, i.e. default.pdf.tex"

	, Option "H" ["here", "nodetect"]
		(NoArg (\opt -> return opt{ optDetect = True }))
		"don't try to detect the book from the current location"

	, Option "b" ["begin"]
		(ReqArg (\arg opt -> case parseLoc arg of
			Left err -> ioError err
			Right loc -> return opt{ optStart = Just loc }) "LOC")
		"the section to start at, i.e. 01 or 2_3_1"

	, Option "e" ["end"]
		(ReqArg (\arg opt -> case parseLoc arg of
			Left err -> ioError err
			Right loc -> return opt{ optEnd = Just loc }) "LOC")
		"the section to compile up to, i.e. 4 or 02_04"

	, Option "n" ["only"]
		(ReqArg (\arg opt -> case parseLoc arg of
			Left err -> ioError err
			Right loc -> return opt{ optStart = Just loc
                                   , optEnd = Just loc }) "LOC")
		"the section to start and end at, compiling only its subsections"

	, Option "h" ["help"]
		(NoArg (\opt -> return opt { optHelp = True}))
		"display this help"
	]

-- | Parse a LOC into Location, i.e. 3_3_1 -> [3, 3, 1]
parseLoc :: String -> Either IOError Location
parseLoc arg = case reads arg of
    [] -> Left $ userError msg
    ((x,""):_) -> Right x
    _ -> Left $ userError msg
    where msg = "Please enter locations in the form of underscore-separated numbers, i.e. 3_1_5"

-- | Main entry point
-- | Parse and ensure command line arguments
-- | Generate configuration environment
main :: IO ()
main = do
	name <- getProgName
	argv <- getArgs
	let (actions, args, optErrors) = getOpt Permute options argv

	let die n = do
		hPutStrLn stderr $ usageInfo name options
		exitWith $ ExitFailure n

	unless (null optErrors) $ do
		mapM_ (\e -> hPutStrLn stderr (name ++ ": " ++ e)) optErrors
		die 2

	let baseOpts = if null args
		then defaultOptions
		else defaultOptions{ optRoot = head args }
	opts <- foldl (>>=) (return baseOpts) actions

	when (optHelp opts) $ do
		putStrLn $ usageInfo name options
		exitWith ExitSuccess

	(normalized, warnings) <- configure opts
	mapM_ (hPutStrLn stderr . show) warnings
	case normalized of
		(Left errs) -> mapM_ (hPutStrLn stderr . show) errs >> die 2
		(Right config) -> execute (optDebug opts) config
		
execute :: Bool -> Config -> IO ()
execute debug config = do
	tgts <- runReaderT targets config
	mapM_ (output debug) tgts

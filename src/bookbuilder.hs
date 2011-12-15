module Main where

import Control.Monad ( when, unless )
import Control.Monad.Reader ( ReaderT(runReaderT) )
import Data.List.Utils ( startswith )
import Data.Maybe ( isNothing )
import Data.String.Utils ( split )
import System.Console.GetOpt
import System.Exit ( exitWith, ExitCode (..) )
import System.Environment ( getArgs, getProgName )
import System.IO ( hPutStrLn, stderr )
-- import Text.Bookbuilder ( Config(..), compile ) TODO
import Text.Bookbuilder
import Text.Regex.Posix ( (=~) )

-- | Data.List utilities
intersects :: (Eq a) => [a] -> [a] -> Bool
intersects xs ys = or [x `elem` ys | x <- xs]

-- | Defaults for command-line configuration
defaultConfig :: Config
defaultConfig = Config
	{ confRoot        = ""
	, confSourceDir   = "src"
	, confTemplateDir = "templates"
	, confTheme       = "common"
	, confOutputDest  = Nothing
	, confDetect      = False
	, confStart       = []
	, confEnd         = []
	, confHelp        = False
	}

-- | A list of functions, each transforming the options data structure
-- | in response to a command-line option.
options :: [OptDescr (Config -> IO Config)]
options =
	[ Option "s" ["src"]
		(ReqArg (\arg opt -> return opt { confSourceDir = arg }) "DIR")
		"directory containing the book source files"

	, Option "p" ["templates"]
		(ReqArg (\arg opt -> return opt { confTemplateDir = arg }) "DIR")
		"directory containing template files"

	, Option "t" ["theme"]
		(ReqArg (\arg opt -> do
			parseTheme arg
			return opt { confTheme = arg }) "THEME")
		"the template theme to use"

	, Option "o" ["output"]
		(ReqArg (\arg opt -> return opt{ confOutputDest = Just arg }) "FILE")
		"destination for the compiled book (stdout if omitted)"

	, Option "d" ["detect"]
		(NoArg (\opt -> return opt{ confDetect = True }))
		"detect the current book and location"

	, Option "m" ["smart"]
		(NoArg (\opt -> return opt{ confOutputDest = Just "" }))
		"determine the output name from the title and the range"

	, Option "b" ["begin"]
		(ReqArg (\arg opt -> parseLoc arg >>= (\ns -> return opt
			{ confStart = ns })) "LOC")
		"the section to start at, i.e. 01 or 02.03.02"

	, Option "e" ["end"]
		(ReqArg (\arg opt -> parseLoc arg >>= (\ns -> return opt
			{ confEnd = ns })) "LOC")
		"the section to compile up to, i.e. 04 or 02.04"

	, Option "n" ["only"]
		(ReqArg (\arg opt -> parseLoc arg >>= (\ns -> return opt
			{ confStart = ns
			, confEnd   = ns})) "LOC")
		"the section to start and end at, compiling only its subsections"

	, Option "h" ["help"]
		(NoArg (\opt -> return opt { confHelp = True}))
		"display this help"
	]

-- | Parse a LOC into [Integer], i.e. 03.03.01 -> [3, 3, 1]
parseLoc :: String -> IO [Integer]
parseLoc arg = if arg =~ "^([0-9]+\\.)*[0-9]+$" :: Bool
	then return $ map read $ split "." arg
	else ioError (userError msg) >> return []
	where msg = "Please enter locations in the form of dot-separated numbers, i.e. 03.01.05"

parseTheme :: String -> IO ()
parseTheme arg = do
	let msg = "Themes may not be blank, start with 'any', nor contain any of: "
	let invalid = "0123456789_-'~."
	let bad = (startswith "any" arg) ||
			  (arg `intersects` invalid) ||
			  (null arg)
	when bad (ioError $ userError $ msg ++ invalid)

-- | Main entry point
-- | Parse and ensure command line arguments
-- | Generate configuration environment
main :: IO ()
main = do
	name <- getProgName
	argv <- getArgs
	let (actions, args, errors) = getOpt Permute options argv

	unless (null errors) $ do
		mapM_ (\e -> hPutStrLn stderr (name ++ ": " ++ e)) errors
		hPutStrLn stderr $ usageInfo name options
		exitWith $ ExitFailure 2

	let baseConfig = if null args
		then defaultConfig
		else defaultConfig{ confRoot = head args }
	config <- foldl (>>=) (return baseConfig) actions

	when (confHelp config) $ do
		putStrLn $ usageInfo name options
		exitWith ExitSuccess
	
	runReaderT compile =<< normalize config

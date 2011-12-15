module Main where

-- TODO: Remember that 00-Something means it's frontmatter

import Control.Monad ( when, unless )
import Control.Monad.Reader ( ReaderT(runReaderT) )
import Data.Maybe ( isNothing )
import Data.String.Utils ( split )
import System.Console.GetOpt
import System.Exit ( exitWith, ExitCode (..) )
import System.Environment ( getArgs, getProgName )
import System.IO ( hPutStrLn, stderr )
-- import Text.Bookbuilder ( Config(..), compile )
import Text.Bookbuilder
import Text.Regex.Posix ( (=~) )

-- | Defaults for command-line configuration
defaultConfig :: Config
defaultConfig = Config
	{ confRoot        = "."
	, confSourceDir   = "src"
	, confTemplateDir = "templates"
	, confOutputDest  = ""
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

	, Option "m" ["templates"]
		(ReqArg (\arg opt -> return opt { confTemplateDir = arg }) "DIR")
		"directory containing template files"

	, Option "o" ["output"]
		(ReqArg (\arg opt -> return opt { confOutputDest = arg }) "FILE")
		"destination for the compiled book"

	, Option "f" ["from"]
		(ReqArg (\arg opt -> parseLoc arg >>= (\ns -> return opt
			{ confStart = ns })) "LOC")
		"the section to start at, i.e. 01 or 02.03.02"

	, Option "t" ["to"]
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
	
	runReaderT compile config

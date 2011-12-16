module Main where

import Control.Monad ( when, unless )
import Control.Monad.Reader ( ReaderT(runReaderT) )
import Data.List.Utils ( startswith )
import Data.List.Split ( splitOneOf )
import Data.Maybe ( isNothing )
import Data.String.Utils ( split )
import System.Console.GetOpt
import System.Exit ( exitWith, ExitCode (..) )
import System.Environment ( getArgs, getProgName )
import System.IO ( hPutStrLn, stderr )
-- import Text.Bookbuilder ( Config(..), compile, normalize )
import Text.Bookbuilder
import qualified Text.Bookbuilder.Location as Location
import Text.Bookbuilder.Location ( Location(Location) )
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
	, confTheme       = "default"
	, confOutputDest  = Nothing
	, confDetect      = False
	, confStart       = Location.empty
	, confEnd         = Location.empty
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
		(ReqArg (\arg opt -> case parseTheme arg of
			Left err -> ioError err
			Right theme -> return opt{ confTheme = theme }) "THEME")
		"the template theme to use"

	, Option "o" ["output"]
		(ReqArg (\arg opt -> return opt{ confOutputDest = Just arg }) "FILE")
		"destination for the compiled book (stdout if omitted)"

	, Option "d" ["detect"]
		(NoArg (\opt -> return opt{ confDetect = True }))
		"detect the current book and location"

	, Option "b" ["begin"]
		(ReqArg (\arg opt -> case parseLoc arg of
			Left err -> ioError err
			Right loc -> return opt{ confStart=loc }) "LOC")
		"the section to start at, i.e. 01 or 2_3_1"

	, Option "e" ["end"]
		(ReqArg (\arg opt -> case parseLoc arg of
			Left err -> ioError err
			Right loc -> return opt{ confEnd=loc }) "LOC")
		"the section to compile up to, i.e. 4 or 02_04"

	, Option "n" ["only"]
		(ReqArg (\arg opt -> case parseLoc arg of
			Left err -> ioError err
			Right loc -> return opt{ confStart=loc, confEnd=loc }) "LOC")
		"the section to start and end at, compiling only its subsections"

	, Option "h" ["help"]
		(NoArg (\opt -> return opt { confHelp = True}))
		"display this help"
	]

-- | Parse a LOC into Location, i.e. 3_3_1 -> [3, 3, 1]
parseLoc :: String -> Either IOError Location
parseLoc arg = if arg =~ "^([0-9]+[,._-|])*[0-9]+$" :: Bool
	then Right $ Location $ map read $ splitOneOf ",._-|" arg
	else Left $ userError msg where
	msg = "Please enter locations in the form of underscore-separated numbers, i.e. 3_1_5"

-- | Parse a template theme, ensuring that the name is valid
parseTheme :: String -> Either IOError String
parseTheme arg = if bad then Left error else Right arg where
	invalidChars = "0123456789._-|"
	invalid = "any" : map (:[]) invalidChars
	bad = null arg || any (`startswith` arg) invalid
	msg = "Themes may not be blank, start with 'any', nor start with any of: "
	error = userError $ msg ++ invalidChars

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

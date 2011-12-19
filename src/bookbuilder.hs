module Main where

-- TODO: Author support
-- TODO: Non-LaTeX template support

import Prelude hiding ( catch )
import Control.Exception ( catch )
import Control.Monad ( when, unless )
import Control.Monad.Reader ( ReaderT(runReaderT) )
import Data.List.Utils ( startswith )
import System.Console.GetOpt
import System.Exit ( exitWith, ExitCode (..) )
import System.Environment ( getArgs, getProgName )
import System.IO ( hPutStrLn, stderr )
import System.IO.Error
    ( isUserError
    , ioeGetLocation
    , ioeGetFileName )
import Text.Bookbuilder ( compile )
import Text.Bookbuilder.Config ( Config(..), normalize, defaultTheme )
import Text.Bookbuilder.Location ( Location(Location) )

-- | Data.List utilities
intersects :: (Eq a) => [a] -> [a] -> Bool
intersects xs ys = or [x `elem` ys | x <- xs]

-- | Defaults for command-line configuration
defaultConfig :: Config
defaultConfig = Config
	{ confRoot        = ""
	, confSourceDir   = "src"
	, confTemplateDir = Nothing
    , confTemplates   = []
	, confTheme       = defaultTheme
	, confOutputDest  = Nothing
	, confDetect      = False
	, confStart       = Location []
	, confEnd         = Location []
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
		(ReqArg (\arg opt -> return opt { confTemplateDir = Just arg }) "DIR")
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
parseLoc arg = case reads arg of
    [] -> Left $ userError msg
    ((x,""):_) -> Right x
    _ -> Left $ userError msg
    where msg = "Please enter locations in the form of underscore-separated numbers, i.e. 3_1_5"

-- | Parse a template theme, ensuring that the name is valid
parseTheme :: String -> Either IOError String
parseTheme arg = if bad then Left err else Right arg where
	invalidChars = "0123456789._-|"
	invalid = "any" : map (:[]) invalidChars
	bad = null arg || any (`startswith` arg) invalid
	msg = "Themes may not be blank, start with 'any', nor start with any of: "
	err = userError $ msg ++ invalidChars

showError :: IOError -> IO ()
showError err | isUserError err = do
    putStr $ "Error: " ++ ioeGetLocation err
    case ioeGetFileName err of
        Nothing -> putStr "\n"
        Just x -> putStrLn $ " (" ++ show x ++ ")"
              | otherwise = print err

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
	
	catch (runReaderT compile =<< normalize config) showError

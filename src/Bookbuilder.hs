module Main where

-- TODO: Remember that 00-Something means it's frontmatter

import Control.Monad ( when, unless )
import Control.Monad.Reader ( ReaderT(runReaderT) )
import System.Console.GetOpt
import System.Exit ( exitWith, ExitCode (..) )
import System.Environment ( getArgs, getProgName )
import System.IO ( hPutStrLn, stderr )
import Text.Bookbuilder ( Config(..), compile )

-- | Defaults for command-line configuration
defaultConfig :: Config
defaultConfig = Config
	{ confRoot        = "."
	, confSourceDir   = "src"
	, confTemplateDir = "templates"
	, confOutputDir   = "build"
	, confCombineTool = Nothing
	, confOmitEmpty   = True
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

	, Option "o" ["build"]
		(ReqArg (\arg opt -> return opt { confOutputDir = arg }) "DIR")
		"destination directory for the compiled book"

	, Option "c" ["combine"]
		(ReqArg (\arg opt -> return opt { confCombineTool = Just arg }) "PROG")
		"use a customized tool to combining files"

	, Option "a" ["all"]
		(NoArg (\opt -> return opt { confOmitEmpty = True }))
		"create stubs for empty directories"

	, Option "h" ["help"]
		(NoArg (\opt -> return opt { confHelp = True}))
		"display this help"
	]

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

	let baseConfig = if (null args)
		then defaultConfig
		else defaultConfig{ confRoot = args !! 0 }
	config <- foldl (>>=) (return baseConfig) actions

	when (confHelp config) $ do
		putStrLn $ usageInfo name options
		exitWith ExitSuccess
	
	runReaderT compile config

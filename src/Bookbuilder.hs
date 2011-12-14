module Main where

import Control.Monad (when, unless)
import System.Console.GetOpt
import System.Exit ( exitWith, ExitCode (..) )
import System.Environment ( getArgs, getProgName )
import System.FilePath
import System.IO ( hPutStrLn, stderr )
-- import System.Directory ( getAppUserDataDirectory, doesFileExist )

-- | Options
-- | Data structure for command line options.
data Opt = Opt
	{ optConfigDir :: String
	, optHelp      :: Bool
	}

-- | Defaults for command-line options.
defaultOpts :: Opt
defaultOpts = Opt
	{ optConfigDir = "config"
	, optHelp      = False
	}

-- | A list of functions, each transforming the options data structure
-- | in response to a command-line option.
options :: [OptDescr (Opt -> IO Opt)]
options =
	[ Option "c" ["config"]
		(ReqArg (\arg opt -> return opt { optConfigDir = arg }) "DIR")
		"the directory within the book containing the config files"
	, Option "h" ["help"]
		(NoArg (\opt -> return opt { optHelp = True}))
		"display this help message"
	]


main :: IO ()
main = do
	argv <- getArgs
	-- let book = getBookPath
	-- We can do this in one line?
	let (actions, args, errors) = getOpt Permute options argv

	unless (null errors) $ do
		name <- getProgName
		mapM_ (\e -> hPutStrLn stderr (name ++ ": " ++ e)) errors
		hPutStrLn stderr $ usageInfo name options
		exitWith $ ExitFailure 2

	opts <- foldl (>>=) (return defaultOpts) actions
	let Opt { optConfigDir = configDir
			, optHelp      = help
			} = opts

	when help $ do
		name <- getProgName
		putStrLn $ usageInfo name options
		exitWith ExitSuccess
	
	putStrLn configDir

	

{-
	type Portion = (String, String)

	reduce :: FilePath -> String -> IO Portion
	reduce root title = do
	...

build :: Tree FilePath -> IO (String, Pandoc)
build (Tree path sections) = (parseTitle path, foldr (join 0) "" sections)

join :: Integer -> Tree FilePath -> Tree FilePath -> IO Pandoc
join n (Tree ta as) (Tree tb bs) = (show n ta as) `joinPandoc` (show n tb bs)

show :: Integer -> FilePath -> [Tree FilePath] -> IO Pandoc
show n path children = do
	template <- getTemplate n
	let title = parseTitle path
	let content = if isDirectory path
		then foldr (join n+1) "" children
		else parse path
	return $ render template title content

...

joinSections :: Tree FilePath -> Tree FilePath -> String
joinSections (Tree t1 s1) (Tree t2 s2) = (showSection t1 s1) ++ (showSection t2 s2)

showSection :: FilePath -> [Tree FilePath] -> String
showSection path [] = "\section{" ++ (parseTitle path) ++ "}" ++ (parse path) ++ "\endsection"
showSection title chapters = "\section{" ++ (parseTitle title) ++ "}" ++ (foldr joinChapters "" chapters) ++ "\endsection"

joinChapters :: Tree FilePath -> Tree FilePath -> String
joinChapters (Tree t1 c1) (Tree t2 c2) = (showChapter t1 c1) ++ (showChapter t2 c2)

showChapter :: FilePath -> [Tree Filepath] -> String
showChapter path [] = "\section{" ++ (parseTitle path) ++ "}" ++ (parse path) ++ "\endsection"
showChapter path cs = Error
-}

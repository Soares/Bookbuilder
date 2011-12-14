module Main where

-- TODO: Remember that 00-Something means it's frontmatter

import Control.Monad ( when, unless, liftM )
import Control.Monad.Trans ( liftIO )
import Control.Monad.Reader ( ReaderT(runReaderT), ask, asks )
import Data.Tree ( Tree(Node), Forest )
import System.Console.GetOpt
import System.Exit ( exitWith, ExitCode (..) )
import System.Environment ( getArgs, getProgName )
import System.FilePath ( combine )
import System.IO ( hPutStrLn, stderr )
-- import System.Directory ( getAppUserDataDirectory, doesFileExist )
import Text.Pandoc ( Pandoc(Pandoc), Meta(Meta) )

-- | Options
-- | Data structure for command line options.
data Config = Config
	{ confRoot        :: FilePath
	, confSourceDir   :: FilePath
	, confTemplateDir :: FilePath
	, confOutputDir   :: FilePath
	, confCombineTool :: Maybe String
	, confOmitEmpty   :: Bool
	, confHelp        :: Bool
	}
type Configged = ReaderT Config

-- | Defaults for command-line options.
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

	let path = fullPath $ if (null args)
		then confRoot defaultConfig
		else args !! 0
	config <- foldl (>>=) (return defaultConfig{ confRoot=path }) actions

	when (confHelp config) $ do
		putStrLn $ usageInfo name options
		exitWith ExitSuccess
	
	runReaderT compile config


-- | File system functions
fullPath p = p
buildTree src = return $ Node src []

-- | Pandoc functions
withDefaultTitle d t = d
getTitle d = ""
render d = ""

-- | Bookbuilder Behavior
parseTitle t = t
reduce t = return $ Pandoc (Meta [] [] []) []

-- | Actual document compilation
compile :: Configged IO ()
compile = do
	root <- asks confRoot
	let askDir = liftM (combine root) . asks
	src <- askDir confSourceDir
	templates <- askDir confTemplateDir
	destdir <- askDir confOutputDir
	omitEmpty <- asks confOmitEmpty

	let title = parseTitle root
	tree <- liftIO $ buildTree src
	content <- liftIO $ reduce tree
	let doc = content `withDefaultTitle` title
	let string = render doc
	let dest = destdir `combine` (getTitle doc)
	liftIO $ writeFile dest string
	

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

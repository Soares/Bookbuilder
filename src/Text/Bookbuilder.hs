module Text.Bookbuilder
	( Config(..)
	, compile ) where

-- TODO: Remember that 00-Something means it's frontmatter

import Control.Monad ( liftM, foldM )
import Control.Monad.Trans ( liftIO )
import Control.Monad.Reader ( ReaderT, ask, asks )
import Data.Tree ( Tree(Node), Forest )
import System.FilePath.Posix
	( combine
	, isRelative
	, normalise
	, takeDirectory )
import System.Directory
	( getCurrentDirectory
	, getDirectoryContents
	, doesDirectoryExist
	, doesFileExist )
import Text.Pandoc ( Pandoc(Pandoc), Meta(Meta) )

-- | Configuration settings
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

-- | Actual document compilation
compile :: Configged IO ()
compile = do
	Config { confRoot        = curDir
	       , confSourceDir   = srcDir
		   , confTemplateDir = templateDir
		   , confOutputDir   = destDir
		   , confCombineTool = combiner
		   , confOmitEmpty   = omitEmpty } <- ask

	root <- liftIO $ bookPath curDir [srcDir]
	let src = root `combine` src
	let name = parseTitle root

	tree <- liftIO $ buildTree src
	content <- reduce tree

	let doc = content `withDefaultTitle` name
	let title = getTitle doc
	let string = render doc
	let dest = foldr combine root [destDir, title]

	liftIO $ writeFile dest string

-- | Monadic if-then-else
ifM :: IO Bool -> IO a -> IO a -> IO a
ifM pred t f = do
	result <- pred
	if result then t else f

-- | Turn a given file path into an absolute path
absPath :: FilePath -> IO FilePath
absPath path | isRelative path = do
	dot <- getCurrentDirectory
	return $ normalise $ dot `combine` path
absPath path | otherwise = return path

-- | Expand the given directory to an absolute directory,
-- | then walk up the path looking for a directory that has
-- | a few requisite subdirectories.
-- | Failing that, use the absolute path
bookPath :: FilePath -> [FilePath] -> IO FilePath
bookPath root lookFor = absPath root >>= bookPath' where
	bookPath' :: FilePath -> IO FilePath
	bookPath' "/" = absPath root
	bookPath' path = ifM (checksOut path)
		(return path)
		(bookPath' $ takeDirectory path)
	checksOut :: FilePath -> IO Bool
	checksOut path = do
		exists <- mapM (doesDirectoryExist . combine path) lookFor
		return $ minimum exists


-- | File system functions
buildTree :: FilePath -> IO (Tree FilePath)
buildTree src = ifM (doesFileExist src)
	(return $ Node src [])
	(do children <- getDirectoryContents src
	    forest <- mapM buildTree children
	    return $ Node src forest)

-- | Pandoc functions
withDefaultTitle d t = d
getTitle d = ""
render d = ""
joinPandoc x y = x

-- | Bookbuilder Behavior
parseTitle t = t
getTemplate n = return ""
renderTemplate t title content = Pandoc (Meta [] [] []) []
parse :: FilePath -> Configged IO Pandoc
parse p = return emptyDoc
emptyDoc = Pandoc (Meta [] [] []) []

reduce :: Tree FilePath -> Configged IO Pandoc
reduce (Node path sections) = do
	contents <- mapM (load 0) sections
	return $ foldr joinPandoc emptyDoc contents

load :: Integer -> Tree FilePath -> Configged IO Pandoc
load n (Node path children) = do
	leaf <- liftIO $ doesFileExist path
	template <- getTemplate n
	let title = parseTitle path
	content <- if leaf then loadLeaf else parse path
	return $ renderTemplate template title content where
	-- Todo: dry this up
	loadLeaf = do
		contents <- mapM (load $ n + 1) children
		return $ foldr joinPandoc emptyDoc contents

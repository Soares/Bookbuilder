module Text.Bookbuilder
	( Config(..)
	, compile ) where

-- TODO: Remember that 00-Something means it's frontmatter

import Control.Monad ( liftM )
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
	content <- liftIO $ reduce tree

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

-- | Bookbuilder Behavior
parseTitle t = t

reduce :: Tree FilePath -> Contextual IO Pandoc
reduce (Tree path sections) = foldM (join 0) "" sections

join :: Integer -> Tree FilePath -> Tree FilePath -> Contextual IO Pandoc
join n (Tree a as) (Tree b bs) = (load n a as) `joinPandoc` (load n b bs)

load :: Integer -> FilePath -> [Tree FilePath] -> Contextual IO Pandoc
load n path children = do
	template <- getTemplate n
	let title = parseTitle path
	let leaf = liftIO $ doesFileExist path
	if leaf then foldM (join $ n + 1) "" children else parse path

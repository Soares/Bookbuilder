module Text.Bookbuilder
	{-
	( Config(..)
	, compile )
	-}
	where

import Char ( toLower )
import Control.Monad ( liftM, liftM2, liftM3, filterM )
import Control.Monad.Loops ( andM, orM )
import Control.Monad.Trans ( liftIO )
import Control.Monad.Reader ( ReaderT, asks )
import Data.List ( sort )
import Data.List.Split ( split, startsWithOneOf )
import Data.List.Utils ( startswith )
import Data.Maybe ( fromJust )
import Data.Tree ( Tree(Node), Forest )
import Data.String.Utils ( join )
import System.Directory
	( getCurrentDirectory
	, getDirectoryContents
	, doesDirectoryExist
	, doesFileExist )
import System.FilePath.Posix
	( combine
	, isRelative
	, makeRelative
	, dropExtension
	, hasExtension
	, takeExtension
	, addExtension
	, splitPath
	, normalise
	, takeDirectory )
import System.Posix.Files
	( getFileStatus
	, fileSize )
import Text.Pandoc
	( Pandoc(Pandoc)
	, Meta(Meta, docTitle)
	, Inline(Str)
	, readers
	, writeLaTeX
	, defaultParserState
	, defaultWriterOptions
	, WriterOptions(..) )
import Text.Regex.Posix ( (=~) )


-- | Configuration options          ====================================
data Config = Config
	{ confRoot        :: FilePath
	, confSourceDir   :: FilePath
	, confTemplateDir :: FilePath
	, confOutputDest  :: FilePath
	, confStart       :: [Integer]
	, confEnd         :: [Integer]
	, confHelp        :: Bool
	} deriving (Eq, Show)


-- | Helper data types
type Configged = ReaderT Config


-- | Helper functions
requiredDirs :: Config -> [FilePath]
requiredDirs conf = [confSourceDir conf]


-- | Configged IO accessors
root :: Configged IO FilePath
root = liftM2 bookPath (asks confRoot) (asks requiredDirs) >>= liftIO

src :: Configged IO FilePath
src = liftM2 combine root (asks confSourceDir)

templates :: Configged IO FilePath
templates = liftM2 combine root (asks confTemplateDir)

dest :: Configged IO FilePath
dest = do
	base <- root
	part <- asks confOutputDest
	file <- if (null part) then defaultDest else return part
	return $ extend $ combine base file
	where extend p | hasExtension p = p
	               | otherwise = addExtension p "tex"

defaultDest :: Configged IO FilePath
defaultDest = do
	title <- liftM parseTitle root
	lower <- asks confStart
	upper <- asks confEnd
	return $ createDest title lower upper

createDest :: String -> [Integer] -> [Integer] -> String
createDest t [] [] = t
createDest t lo hi = t ++ "." ++ (sep lo) ++ "." ++ (sep hi) where
	sep list = if (null list) then "-" else join "-" $ map show list


-- | Control.Monad utilities        ====================================

ifM :: Monad m => m Bool -> m a -> m a -> m a
ifM p x y = p >>= (\r -> if r then x else y)

notM :: Monad m => m Bool -> m Bool
notM = liftM not


-- | System.FilePath utilities      ====================================

ls :: FilePath -> IO [FilePath]
ls = fmap (sort . filter isVisible) . getDirectoryContents
	where isVisible = not . startswith "."

fileHasData :: FilePath -> IO Bool
fileHasData file = fmap ((> 0) . fileSize) (getFileStatus file)

absPath :: FilePath -> IO FilePath
absPath path | isRelative path = do
	dot <- getCurrentDirectory
	return $ normalise $ dot `combine` path
absPath path = return path



-- | Text.Pandoc utilities          ====================================

pandocName :: FilePath -> String
pandocName p = case takeExtension (map toLower p) of
    ".xhtml"    -> "html"
    ".html"     -> "html"
    ".htm"      -> "html"
    ".tex"      -> "latex"
    ".latex"    -> "latex"
    ".ltx"      -> "latex"
    ".rst"      -> "rst"
    ".lhs"      -> "markdown+lhs"
    ".textile"  -> "textile"
    ".native"   -> "native"
    ".json"     -> "json"
    _           -> "markdown"

pandocify :: String -> String -> Configged IO String
pandocify path contents = do
	template <- getTemplate path
	srcDir <- src
	base <- root
	let rawTitle = if path == srcDir then base else path
	let title = parseTitle rawTitle
	let pname = pandocName path
	-- TODO: fromJust is bad
	let parse = (fromJust $ lookup pname readers) defaultParserState
	let render = writeLaTeX defaultWriterOptions
		{ writerStandalone       = True
		, writerTemplate         = template
		, writerChapters         = True }
	return $ render $ (parse contents) `withDefaultTitle` title

withDefaultTitle :: Pandoc -> String -> Pandoc
withDefaultTitle (Pandoc (Meta [] as d) bs) t = (Pandoc (Meta [Str t] as d) bs)
withDefaultTitle doc _ = doc



-- | Bookbuilder file locations     ====================================

-- | This is a wee bit hackish because ordering lists is *almost* correct,
-- | except we always descend into children if the parent was in range
-- | and we treat [] as unbounded on both sides. This leads to:
-- |	[3, 3] <= [3, 3, 1] <= [3, 3]
-- |	[] <= [1] <= []
-- | Which is why we aren't using an Ord instance.
-- |
-- | WARNING: This will also say that [3, 3] <= [17, 17, 1] <= [3, 3],
-- | only use this to *decide* whether to descend. The result is only
-- | valid if the parent location was already in range.
locationInRange :: [Integer] -> [Integer] -> [Integer] -> Bool
locationInRange loc low high = loc >= low && loc `lte` high where
	loc `lte` bar | null bar = True
	              | loc <= bar = True
	              | length loc > length bar = True
	              | otherwise = False

getLocation :: FilePath -> Configged IO [Integer]
getLocation path = do
	dirs <- liftM (splitPath . makeRelative path) src
	return $ map locationIndex dirs

locationIndex :: String -> Integer
locationIndex name = if null nums then 1 else read nums where
	nums = name =~ "^([0-9]+)"



-- | Bookbuilder behavior           ====================================

parseTitle :: FilePath -> String
parseTitle = deCamel . dropExtension . dropIndex where
	dropIndex = dropWhile $ flip elem "_.-0123456789"
	deCamel = join " " . split (startsWithOneOf ['A'..'Z'])

-- Todo
getTemplate :: FilePath -> Configged IO String
getTemplate path = return "$body$"



-- | Bookbuilder file discovery     ====================================

-- | Expand the given directory to an absolute directory, then walk up the
-- | path looking for a directory that has the requisite subdirectories.
-- | Fall back to the absolute path on failure.
bookPath :: FilePath -> [FilePath] -> IO FilePath
bookPath root lookFor = absPath root >>= search where
	search "/" = absPath root
	search path = ifM (checksOut path)
		(return path)
		(search $ takeDirectory path)
	checksOut path = andM $ map (isThere . combine path) lookFor
	isThere p = liftM2 (||) (doesDirectoryExist p) (doesFileExist p)

-- | Build a tree out of the filepaths in the book's source directory
buildTree :: FilePath -> IO (Tree FilePath)
buildTree path = ifM (doesFileExist path)
	(return $ Node path [])
	(do children <- ls path
	    forest <- mapM (buildTree . combine path) children
	    return $ Node path forest)



-- | Bookbuilder tree manipulation  ====================================

prune :: Tree FilePath -> Configged IO (Tree FilePath)
prune (Node p cs) = fmap (Node p) (filterM keep =<< mapM prune cs)

keep :: Tree FilePath -> Configged IO Bool
keep node = liftM2 (&&) (hasContent node) (isInRange node)

hasContent :: Tree FilePath -> Configged IO Bool
hasContent (Node p []) = liftIO $ andM [doesFileExist p, fileHasData p]
hasContent _ = return True

isInRange :: Tree FilePath -> Configged IO Bool
isInRange (Node t _) = liftM3 locationInRange
	(getLocation t) (asks confStart) (asks confEnd)



-- | Bookbuilder compilation        ====================================

flatten :: Tree FilePath -> Configged IO String
flatten (Node path children) = do
	leaf <- liftIO $ doesFileExist path
	contents <- if leaf
		then liftIO $ readFile path
		else liftM concat $ mapM flatten children
	pandocify path contents

compile :: Configged IO ()
compile = do
	-- TODO: reduntant title-grabbing
	tree <- liftIO . buildTree =<< src
	content <- flatten tree
	write <- liftM writeFile dest
	liftIO $ write content

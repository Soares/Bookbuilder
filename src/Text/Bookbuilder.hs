module Text.Bookbuilder
	{- TODO
	( Config(..)
	, normalize
	, compile )
	-}
	where

-- TODO: treat frontmatter special
-- TODO: handle ioerrors in template name parsing
-- TODO: allow trailing newline supression (why is \endchapter not newlined?)
-- TODO: TEMPLATES WILL NO LONGER ADD NEWLINES
-- TODO: hlint

import Control.Monad ( liftM, liftM2, liftM3, filterM, when )
import Control.Monad.Loops ( andM, orM )
import Control.Monad.Trans ( liftIO )
import Control.Monad.Reader ( ReaderT, asks, local )
import Data.Char ( toLower )
import Data.Ord ( Ordering(LT, EQ, GT), compare )
import Data.List ( sort, nub )
import Data.List.Split ( split, splitOn, startsWithOneOf )
import Data.List.Utils ( startswith, endswith )
import Data.Maybe ( fromJust, isNothing )
import Data.Tree ( Tree(Node), Forest )
import Data.String.Utils ( join )
import System.Directory
	( getCurrentDirectory
	, getDirectoryContents
	, canonicalizePath
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
	, normalise
	, splitPath
	, joinPath
	, takeFileName
	, dropTrailingPathSeparator
	, addTrailingPathSeparator
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
	, confTheme       :: String
	, confOutputDest  :: Maybe FilePath
	, confDetect      :: Bool
	, confStart       :: [Integer]
	, confEnd         :: [Integer]
	, confHelp        :: Bool
	} deriving (Eq, Show)


-- | Helper data types
type Configged = ReaderT Config


-- | Configged IO accessors
src :: Configged IO FilePath
src = liftM2 combine (asks confRoot) (asks confSourceDir)

templates :: Configged IO FilePath
templates = liftM2 combine (asks confRoot) (asks confTemplateDir)

dest :: FilePath -> Configged IO FilePath
dest path = do
	base <- liftM2 combine (asks confRoot) (return path)
	isDir <- liftIO $ doesDirectoryExist base
	file <- if null path || isDir then defaultDest else return ""
	return $ offerExtension "tex" $ combine base file

defaultDest :: Configged IO FilePath
defaultDest = do
	title <- liftM parseTitle (asks confRoot)
	lower <- asks confStart
	upper <- asks confEnd
	return $ createDest title lower upper

createDest :: String -> [Integer] -> [Integer] -> String
createDest t [] [] = t
createDest t lo hi | null hi = join "--" [t, sep lo]
                   | null lo = join "--" [t, sep hi]
                   | lo == hi = join "-" [t, sep lo]
                   | otherwise = join "-" [t, sep lo, sep hi]
	where sep = join "_" . map show



-- | Control.Monad utilities        ====================================

ifM :: Monad m => m Bool -> m a -> m a -> m a
ifM p x y = p >>= (\r -> if r then x else y)

notM :: Monad m => m Bool -> m Bool
notM = liftM not


-- | Data.List utilities            ====================================

dropLast :: [a] -> [a]
dropLast (x:[]) = []
dropLast (x:xs) = x:dropLast xs

-- | System.FilePath utilities      ====================================

ls :: FilePath -> IO [FilePath]
ls = fmap (sort . filter isVisible) . getDirectoryContents
	where isVisible = not . startswith "."

exists :: FilePath -> IO Bool
exists p = liftM2 (||) (doesDirectoryExist p) (doesFileExist p)

fileHasData :: FilePath -> IO Bool
fileHasData file = fmap ((> 0) . fileSize) (getFileStatus file)

offerExtension :: String -> FilePath -> FilePath
offerExtension ext p | hasExtension p = p
                     | otherwise = addExtension p ext

absPath :: FilePath -> IO FilePath
absPath path | isRelative path = do
	dot <- getCurrentDirectory
	return $ normalise $ dot `combine` path
absPath path = return $ normalise path



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
	base <- ifM isTop (asks confRoot) (return path)
	template <- getTemplate base
	let title = parseTitle base
	let pname = pandocName path
	let parse = (fromJust $ lookup pname readers) defaultParserState
	let render = writeLaTeX defaultWriterOptions
		-- TODO: add writer variables
		-- parent, book, count, total,
		-- N, parentN, countN, totalN
		{ writerStandalone       = True
		, writerTemplate         = template
		, writerChapters         = True }
	return $ render $ parse contents `withDefaultTitle` title
	where isTop = liftM2 (==) src (return path)

withDefaultTitle :: Pandoc -> String -> Pandoc
withDefaultTitle (Pandoc (Meta [] as d) bs) t = Pandoc (Meta [Str t] as d) bs
withDefaultTitle doc _ = doc



-- | Bookbuilder file locations     ====================================

-- | This is a wee bit hackish because ordering lists is *almost* correct,
-- | except we always descend into children if the parent was in range
-- | and we treat [] as unbounded on both sides. This leads to:
-- |    [3] >= [3, 1]
-- |	[3, 3] <= [3, 3, 1] <= [3, 3]
-- |	[] <= [1] <= []
-- | Which is why we aren't using an Ord instance.
-- |
-- | WARNING: This will also say that [3, 3] <= [17, 17, 1] <= [3, 3],
-- | only use this to *decide* whether to descend. The result is only
-- | valid if the parent location was already in range.
locationInRange loc low high = loc `gte` low && loc `lte` high where
	gte = is (>=)
	lte = is (<=)
	is op loc bar | null bar = True -- Null will do whatever you want
	              | op loc bar = True -- It already works
				  | loc == take (length loc) bar = True -- We must descend
				  | take (length bar) loc == bar = True -- We must descend
				  | otherwise = False

getLocation :: FilePath -> Configged IO [Integer]
getLocation path = do
	root <- asks confRoot
	if root == path
		then return []
		else return . map locationIndex . splitPath =<<
			liftM2 makeRelative src (return path)

locationIndex :: String -> Integer
locationIndex name = if null nums then 1 else read nums where
	nums = name =~ "^([0-9]+)"

pathIndex :: FilePath -> Integer
pathIndex = locationIndex . takeFileName . dropTrailingPathSeparator



-- | Bookbuilder parsing            ====================================

parseTitle :: FilePath -> String
parseTitle = deCamel . dropExtension . dropIndex . filePart where
	filePart = takeFileName . dropTrailingPathSeparator
	dropIndex = dropWhile $ flip elem "_.-0123456789"
	deCamel = join " " . split (startsWithOneOf ['A'..'Z'])



-- | Bookbuilder templates          ====================================

data Template = Template
	{ tmplSource  :: String
	, tmplMatches :: [Maybe [Integer]]
	} deriving (Eq, Show)

instance Ord Template where
	t1 <= t2 | len t1 == len t2 = cmp (tmplMatches t1) (tmplMatches t2)
	         | otherwise = len t1 < len t2 where
		len = length . tmplMatches
		cmp [] _ = True
		cmp (_:xs) (Nothing:ys) = True
		cmp (Nothing:xs) (_:ys) = False
		cmp (Just x:xs) (Just y:ys) | length x == length y = cmp xs ys
		                            | otherwise = length x < length y

buildTemplate :: FilePath -> IO Template
buildTemplate path = do
	content <- readFile path
	let parts = dropLast $ splitOn "-" $ takeFileName path
	return $ Template (content ++ "\n") (map parseOption parts)

parseOption :: String -> Maybe [Integer]
parseOption path = do
	groups <- mapM parseGroup $ splitOn "'" path
	return $ nub $ sort $ concat groups

parseGroup :: String -> Maybe [Integer]
parseGroup = parseRange . splitOn "~"

parseRange :: [String] -> Maybe [Integer]
parseRange [] = Just []
parseRange ["_"] = Nothing
parseRange [x] = Just [read x]
parseRange (x:"":_) = Nothing
parseRange (x:"_":_) = Nothing
parseRange (x:y:_) = Just [(low x)..(read y)] where
	low z = if null z || (z == "_") then 0 else read z

templateList :: Configged IO [Template]
templateList = do
	dir <- templates
	theme <- asks confTheme
	let suffix = offerExtension "tex" theme
	files <- liftIO $ ls dir
	let tmpl p = endswith suffix p && not $ startswith "any" p
	let names = map (combine dir) files
	ts <- liftIO $ mapM buildTemplate $ filter tmpl names
	return $ sort ts

findTemplate :: String -> [Integer] -> [Template] -> String
findTemplate fb loc ts = get $ filter (templateTakes loc) ts
	where get [] = fb
	      get (x:xs) = tmplSource x

templateTakes :: [Integer] -> Template -> Bool
templateTakes xs (Template _ os) = matches xs os where
	matches [] y = y == []
	matches _ [] = False
	matches (x:xs) (Nothing:os) = matches xs os
	matches (x:xs) (Just o:os) = (x `elem` o) && matches xs os

fallbackTemplate :: Configged IO String
fallbackTemplate = do
	dir <- templates
	theme <- asks confTheme
	let file = combine dir $ "any" ++ offerExtension "tex" theme
	read <- liftIO $ doesFileExist file
	if read then liftIO $ readFile file else return "$body$\n"

getTemplate :: FilePath -> Configged IO String
getTemplate path = do
	templates <- templateList
	location <- getLocation path
	fb <- fallbackTemplate
	return $ findTemplate fb location templates



-- | Bookbuilder file discovery     ====================================

absify :: Config -> IO Config
absify conf = do
	root <- absPath (confRoot conf)
	return $ conf{ confRoot=root }

normalize :: Config -> IO Config
normalize conf = if not $ confDetect conf then absify conf else do
	let cur = confRoot conf
	let src = confSourceDir conf
	(root, range) <- detect cur [src]
	return conf{confRoot=root, confStart=range, confEnd=range}

-- | Expand the given directory to an absolute directory, then walk up the
-- | path looking for a directory that has the requisite subdirectories.
-- | Fall back to the absolute path on failure.
detect :: FilePath -> [FilePath] -> IO (FilePath, [Integer])
detect start lookFor = do
	normpath <- canonicalizePath =<< absPath start
	detection <- search normpath []
	case detection of
		Nothing -> return (normpath, [])
		Just (root, srcnum:loc) -> return (root, loc)
	where search "/" _ = return Nothing
	      search path xs = ifM (checksOut path)
			(return $ Just (path, xs))
			(search (takeDirectory path) (pathIndex path : xs))
	      checksOut path = andM $ map (exists . combine path) lookFor

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
isInRange (Node t _) = liftM3 locationInRange (getLocation t) (asks confStart) (asks confEnd)



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
	tree <- liftIO . buildTree =<< src
	content <- flatten =<< prune tree
	destPart <- asks confOutputDest
	write <- case destPart of
		Nothing -> return putStr
		Just path -> liftM writeFile (dest path)
	liftIO $ write content

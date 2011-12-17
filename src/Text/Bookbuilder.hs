module Text.Bookbuilder
	( Config(..)
	, normalize
	, compile
	) where

-- TODO: Allow src to be more than one level
-- TODO: Test on only file
-- TODO: Test on no src
-- TODO: Test on leveld/src
-- TODO: x <- a; y <- b IS NOT LAZY

import Control.Monad ( liftM2, liftM3, filterM, unless )
import Control.Monad.Loops ( andM )
import Control.Monad.Trans ( liftIO )
import Control.Monad.Reader ( ReaderT, asks )
import Data.Char ( toLower )
import Data.Functor ( (<$>) )
import Data.List ( sort )
import Data.List.Split ( split, startsWithOneOf )
import Data.List.Utils ( startswith )
import Data.Maybe
	( fromJust
	, catMaybes
	, fromMaybe
	, isNothing )
import Data.Tree ( Tree(Node) )
import Data.String.Utils ( join )
import System.Directory
	( getCurrentDirectory
	, getDirectoryContents
	, canonicalizePath
	, doesDirectoryExist
	, doesFileExist )
import System.FilePath.Posix
	( (</>)
	, (<.>)
	, isDrive
	, isRelative
	, makeRelative
	, dropExtension
	, hasExtension
	, takeExtension
	, normalise
	, splitPath
	, takeFileName
	, dropTrailingPathSeparator
	, takeDirectory )
import System.IO.Error
	( mkIOError
	, userErrorType )
import System.Posix.Files
	( getFileStatus
	, fileSize )
import Text.Pandoc
	( Pandoc(Pandoc)
	, Meta(Meta)
	, Inline(Str)
	, readers
	, writeLaTeX
	, defaultParserState
	, defaultWriterOptions
	, WriterOptions(..) )
import Text.Printf ( printf )

import qualified Text.Bookbuilder.Location as Location
import Text.Bookbuilder.Location ( Location(Location) )
import qualified Text.Bookbuilder.Template as Template
import Text.Bookbuilder.Template ( Template )


-- | Configuration options          ====================================
data Config = Config
	{ confRoot        :: FilePath
	, confSourceDir   :: FilePath
	, confTemplateDir :: Maybe FilePath
	, confTheme       :: String
	, confOutputDest  :: Maybe FilePath
	, confDetect      :: Bool
	, confStart       :: Location
	, confEnd         :: Location
	, confHelp        :: Bool
	} deriving (Eq, Show)


-- | Configuration helpers
type Configged = ReaderT Config

-- | Configuration local structure
src :: Config -> FilePath
src conf = normalise $ confRoot conf </> confSourceDir conf

templates :: Config -> Maybe FilePath
templates conf = normalise . (root </>) <$> templates where
	root = confRoot conf
	templates = confTemplateDir conf

dest :: FilePath -> Configged IO FilePath
dest path = do
	base <- (</> path) <$> asks confRoot
	isDir <- liftIO $ doesDirectoryExist base
	smartName <- asks defaultDest
	let file = if null path || isDir then base </> smartName else base
	return $ offerExtension "tex" file

defaultDest :: Config -> FilePath
defaultDest conf = destName title (confStart conf) (confEnd conf)
	where title = parseTitle $ confRoot conf

destName :: String -> Location -> Location -> String
destName t wlo whi = join "-" (t:parts (unwrap wlo) (unwrap whi)) where
	unwrap = Location.list
	parts [] [] = []
	parts lo hi = if lo == hi then [sep lo] else [sep lo, sep hi]
	sep = join "_" . map show



-- | Prelude utilities              ====================================

contains :: (Ord a) => a -> a -> a -> Bool
contains lo hi x = x >= lo && x <= hi


-- | Control.Monad utilities        ====================================

ifM :: Monad m => m Bool -> m a -> m a -> m a
ifM p x y = p >>= (\r -> if r then x else y)



-- | System.FilePath utilities      ====================================

ls :: FilePath -> IO [FilePath]
ls path = (map (path </>) . sort . filter isVisible) <$>
	getDirectoryContents path

isVisible :: FilePath -> Bool
isVisible = not . startswith "." . takeFileName

exists :: FilePath -> IO Bool
exists p = liftM2 (||) (doesDirectoryExist p) (doesFileExist p)

fileHasData :: FilePath -> IO Bool
fileHasData file = fmap ((> 0) . fileSize) (getFileStatus file)

offerExtension :: String -> FilePath -> FilePath
offerExtension ext p | hasExtension p = p
                     | otherwise = p <.> ext

canonicalize :: FilePath -> IO FilePath
canonicalize = (canonicalizePath =<<) . unrel where
	unrel p | isRelative p = (</> p) <$> getCurrentDirectory
	        | otherwise = return p



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
	template <- Template.source <$> getTemplate base
	let title = parseTitle base
	let pname = pandocName base
	let parse = (fromJust $ lookup pname readers) defaultParserState
	let render = writeLaTeX defaultWriterOptions
		-- TODO: add writer variables
		--     parent, book, count, total,
		--     N, parentN, countN, totalN
		--     fontsize, author, etc.
		{ writerStandalone       = True
		, writerTemplate         = template
		, writerChapters         = True }
	return $ render $ parse contents `withDefaultTitle` title
	where isTop = (path ==) <$> asks src

withDefaultTitle :: Pandoc -> String -> Pandoc
withDefaultTitle (Pandoc (Meta [] as d) bs) t = Pandoc (Meta [Str t] as d) bs
withDefaultTitle doc _ = doc



-- | Bookbuilder file locations     ====================================

getLocation :: FilePath -> Configged IO Location
getLocation path = do
	path <- (liftM2 makeRelative) (asks src) (return path)
	return $ pathLocation path

pathLocation :: FilePath -> Location
pathLocation = Location . map pathIndex . splitPath where
	pathIndex = takeInt . takeFileName . dropTrailingPathSeparator
	takeInt = fromMaybe 1 . first . (map fst) . reads
	first xs = if null xs then Nothing else Just $ head xs



-- | Bookbuilder parsing            ====================================

parseTitle :: FilePath -> String
parseTitle = deCamel . dropExtension . dropIndex . filePart where
	filePart = takeFileName . dropTrailingPathSeparator
	dropable = Location.separators ++ ['0'..'9']
	dropIndex = dropWhile (`elem` dropable)
	deCamel = join " " . split (startsWithOneOf ['A'..'Z'])



-- | Bookbuilder templates          ====================================

templateList :: Configged IO [Template]
templateList = asks templates >>= templateList' where
	templateList' :: Maybe FilePath -> Configged IO [Template]
	templateList' Nothing = return []
	templateList' (Just dir) = do
		files <- liftIO $ ls dir
		unfiltered <- liftIO $ mapM Template.fromFile files
		let candidates = catMaybes unfiltered
		let isOurs t = (Template.theme t ==) <$> asks confTheme
		sort <$> filterM isOurs candidates

findTemplate :: Location -> [Template] -> Maybe Template
findTemplate _ [] = Nothing
findTemplate loc (t:ts) | Template.matches t $ Location.list loc = Just t
                        | otherwise = findTemplate loc ts

getTemplate :: FilePath -> Configged IO Template
getTemplate path = do
	listing <- templateList
	location <- getLocation path
	return $ fromMaybe Template.fallback $ findTemplate location listing



-- | Bookbuilder user errors        ====================================

confError :: String -> Maybe FilePath -> IOError
confError msg = mkIOError userErrorType msg Nothing

errConfNoTemplates = confError msg . templates
	where msg = "Can not find template directory" 

errConfNotABook = confError msg . Just . src
	where msg = "Can not find book source"



-- | Bookbuilder file discovery     ====================================

normalize :: Config -> IO Config
normalize = (check =<<) . (setTemplates =<<) . setRoot

check :: Config -> IO Config
check conf = do
	haveSources <- exists $ src conf
	unless haveSources (ioError $ errConfNotABook conf)
	haveTemplates <- existsIfJust $ templates conf
	unless haveTemplates (ioError $ errConfNoTemplates conf)
	return conf
	where existsIfJust = fromMaybe (return True) . (exists <$>)

setRoot :: Config -> IO Config
setRoot conf = if confDetect conf then detected else canonized where
	getCanonRoot = canonicalize $ confRoot conf
	detected = do
		old <- getCanonRoot
		(root, range) <- detect old (confSourceDir conf)
		return (set root){ confStart=range, confEnd=range }
	canonized = set <$> getCanonRoot
	set root = conf{ confRoot = root }

setTemplates :: Config -> IO Config
setTemplates conf = do
	let given = confTemplateDir conf
	defaultWouldWork <- exists $ confRoot conf </> "templates"
	return $ if isNothing given && defaultWouldWork
		then conf{ confTemplateDir = Just "templates" }
		else conf

-- | Expand the given directory to an absolute directory, then walk up the
-- | path looking for a directory that has the requisite subdirectories.
-- | Fall back to the absolute path on failure.
detect :: FilePath -> FilePath -> IO (FilePath, Location)
detect start sourceDir = do
	cur <- canonicalize start
	root <- unwrap =<< cur `ancestorWith` sourceDir
	let sources = root </> sourceDir
	return (root, pathLocation $ makeRelative sources cur) where
		unwrap (Just root) = return root
		unwrap Nothing = ioError $ userError $ printf msg start sourceDir
		msg = "Could not detect book from %s (looked for %s)"

ancestorWith :: FilePath -> FilePath -> IO (Maybe FilePath)
ancestorWith path child = checksOut >>= ancestorWith' where
	checksOut = exists $ path </> child
	canAscend = path /= takeDirectory path
	next = takeDirectory path
	ancestorWith' True = return $ Just path
	ancestorWith' False | canAscend = ancestorWith next child
	                    | otherwise = return Nothing

-- | Build a tree out of the filepaths in the book's source directory
buildTree :: FilePath -> IO (Tree FilePath)
buildTree path = ifM (doesFileExist path)
	(return $ Node path [])
	(Node path <$> (mapM buildTree =<< ls path))



-- | Bookbuilder tree manipulation  ====================================

prune :: Tree FilePath -> Configged IO (Tree FilePath)
prune (Node p cs) = Node p <$> (filterM keep =<< mapM prune cs)

keep :: Tree FilePath -> Configged IO Bool
keep node = liftM2 (&&) (liftIO $ hasContent node) (isInRange node)

hasContent :: Tree FilePath -> IO Bool
hasContent (Node p []) = andM [doesFileExist p, fileHasData p]
hasContent _ = return True

isInRange :: Tree FilePath -> Configged IO Bool
isInRange (Node t _) = liftM3 contains (asks confStart) (asks confEnd) (getLocation t)


-- | Bookbuilder compilation        ====================================

flatten :: Tree FilePath -> Configged IO String
flatten (Node path children) = do
	leaf <- liftIO $ doesFileExist path
	contents <- if leaf
		then liftIO $ readFile path
		else concat <$> mapM flatten children
	pandocify path contents

compile :: Configged IO ()
compile = do
	tree <- liftIO =<< buildTree <$> asks src
	content <- flatten =<< prune tree
	destPart <- asks confOutputDest
	write <- case destPart of
		Nothing -> return putStr
		Just path -> writeFile <$> dest path
	liftIO $ write content

module Text.Bookbuilder.Config
	( Config(..)
	, Configged
	, normalize
	, rootDir
	, srcDir
	, templateDir
	, range
	, dest
	, operativePart
	, template
	, location
	, defaultTemplatesDir
	, defaultTheme
	) where

import Control.Arrow ( (&&&) )
import Control.Monad ( ap, unless )
import Control.Monad.Loops ( allM, concatM )
import Control.Monad.Reader ( ReaderT )
import Data.Functor ( (<$>) )
import Data.List ( sort )
import Data.Maybe ( catMaybes, fromMaybe )
import Data.String.Utils ( join )
import System.Directory ( doesDirectoryExist )
import System.FilePath.Posix
	( (</>)
	, normalise
	, takeFileName
	, dropTrailingPathSeparator
	, takeDirectory )
import System.IO.Error ( mkIOError , userErrorType )
import Text.Bookbuilder.FilePath
	( ls
	, exists
	, pathTitle
	, offerExtension
	, canonicalize
	, pathLocation
	, from )
import Text.Bookbuilder.Location ( Location, toList )
import Text.Bookbuilder.Template
	( Template
	, fallback
	, theme
	, fromFile
	, matches )
import Text.Printf ( printf )


data Config = Config
	{ confRoot        :: FilePath
	, confSourceDir   :: FilePath
	, confTemplateDir :: Maybe FilePath
	, confTemplates   :: [Template]
	, confTheme       :: String
	, confOutputDest  :: Maybe FilePath
	, confDetect      :: Bool
	, confStart       :: Location
	, confEnd         :: Location
	, confHelp        :: Bool
	} deriving (Eq, Show)

type Configged = ReaderT Config

rootDir :: Config -> FilePath
rootDir = confRoot

srcDir :: Config -> FilePath
srcDir = ap fromRoot confSourceDir

templateDir :: Config -> Maybe FilePath
templateDir = ap maybeFromRoot confTemplateDir

range :: Config -> (Location, Location)
range = confStart &&& confEnd

dest :: Config -> Maybe FilePath
dest = ap maybeFromRoot confOutputDest

template :: FilePath -> Config -> Template
template path conf = fromMaybe fallback match
	where match = findTemplate (location path conf) (confTemplates conf)

operativePart :: FilePath -> Config -> String
operativePart path conf | path == srcDir conf = clip $ rootDir conf
                        | otherwise = clip path
	where clip = takeFileName . dropTrailingPathSeparator

location :: FilePath -> Config -> Location
location path = pathLocation . (path `from`) . srcDir

normalize :: Config -> IO Config
normalize = concatM [ setRoot
                    , setTemplateDir
                    , setDestination
                    , setTemplateList
                    , check ]


-- Root discovery

-- | Walk up the current directory looking for what looks like the root of the
-- | book, returning the root of the book and the Location of the original path
-- | relative to the discovered root. Throws UserError if detection fails.
detect :: FilePath -> FilePath -> [FilePath] -> IO (FilePath, Location)
detect start source others = locate <$> findRoot where
	requisites = source : others
	locate root = (root, pathLocation $ start `from` (root </> source))
	findRoot = start `ancestorWith` requisites >>= unwrapOrThrow
	unwrapOrThrow Nothing = ioError $ errCantDetect requisites start
	unwrapOrThrow (Just x) = return x

ancestorWith :: FilePath -> [FilePath] -> IO (Maybe FilePath)
ancestorWith path children = checksOut >>= ancestorWith' where
	ancestorWith' True = return $ Just path
	ancestorWith' False | canAscend = ancestorWith next children
	                    | otherwise = return Nothing
	checksOut = allM (exists . (path </>)) children
	canAscend = path /= takeDirectory path
	next = takeDirectory path

setRoot :: Config -> IO Config
setRoot conf = canonicalize (confRoot conf) >>= setRoot' where
	requisites = catMaybes [confTemplateDir conf]
	set (root, loc) = conf{ confRoot = root, confStart = loc, confEnd = loc }
	setRoot' root = if confDetect conf then detected else canonized where
		detected = set <$> (detect root (confSourceDir conf) requisites)
		canonized = return conf{ confRoot = root }


-- Template destination selection
defaultTemplatesDir :: String
defaultTemplatesDir = "templates"

setTemplateDir :: Config -> IO Config
setTemplateDir conf = maybeDefault $ confTemplateDir conf where
	maybeDefault Nothing = choose <$> wouldDefaultWork
	maybeDefault _ = return conf
	wouldDefaultWork = exists $ confRoot conf </> defaultTemplatesDir
	choose defaultWorks = if defaultWorks then defaulted else conf
	defaulted = conf{ confTemplateDir = Just defaultTemplatesDir }


-- Output destination selection
destName :: String -> Location -> Location -> String
destName t wlo whi = join "-" (t:parts (toList wlo) (toList whi)) where
	parts [] [] = []
	parts lo hi = if lo == hi then [sep lo] else [sep lo, sep hi]
	sep = join "_" . map show

setDestination :: Config -> IO Config
setDestination conf = selectAndSet $ confOutputDest conf where
	selectAndSet = maybe (return conf) (fmap set . select)
	set out = conf{ confOutputDest = Just $ offerExtension "tex" out }
	altName = destName (pathTitle root) (confStart conf) (confEnd conf)
	root = confRoot conf
	select "" = return altName
	select part = select' <$> doesDirectoryExist (root </> part) where
		select' isDir = if isDir then part </> altName else part


-- Template discovery
defaultTheme :: String
defaultTheme = "default"

templateList :: Config -> IO [Template]
templateList conf = maybe (return []) findIn (templateDir conf) where
	findIn dir = select <$> (mapM fromFile =<< ls dir)
	select = sort . filter isGood . catMaybes
	themes = [confTheme conf, defaultTheme]
	isGood = (`elem` themes) . theme

findTemplate :: Location -> [Template] -> Maybe Template
findTemplate _ [] = Nothing
findTemplate loc (t:ts) | matches t $ toList loc = Just t
                        | otherwise = findTemplate loc ts

setTemplateList :: Config -> IO Config
setTemplateList conf = set <$> templateList conf
	where set ts = conf{ confTemplates = ts }


-- Verification
check :: Config -> IO Config
check conf = do
	haveSources <- exists $ srcDir conf
	unless haveSources (ioError $ errNotABook conf)
	haveTemplates <- existsIfJust $ templateDir conf
	unless haveTemplates (ioError $ errNoTemplates conf)
	return conf
	where existsIfJust = maybe (return True) exists


-- Errors
errCantDetect :: [FilePath] -> FilePath -> IOError
errCantDetect sources origin = confError (msg $ show sources) (Just origin)
	where msg = printf "Could not detect book (looked for directories: %s)"

errNoTemplates :: Config -> IOError
errNoTemplates = confError msg . templateDir
	where msg = "Can not find template directory" 

errNotABook :: Config -> IOError
errNotABook = confError msg . Just . srcDir
	where msg = "Can not find book source"


-- Helpers
confError :: String -> Maybe FilePath -> IOError
confError msg = mkIOError userErrorType msg Nothing

fromRoot :: Config -> FilePath -> FilePath
fromRoot conf path = normalise $ confRoot conf </> path

maybeFromRoot :: Config -> Maybe FilePath -> Maybe FilePath
maybeFromRoot = fmap . fromRoot

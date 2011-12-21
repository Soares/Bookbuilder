module Text.Bookbuilder.Profile
	( Profile
	, ProfileWarning
	, filename
	, buildFormat
	, load
	, template
	, lift
	, write
	, styleFile
	, resourceFile
	) where

import Control.Monad ( when )
import Control.Monad.State ( StateT, runStateT, modify )
import Control.Monad.Trans ( liftIO )
import Data.Functor ( (<$>) )
import Data.List ( nub, sort )
import Data.List.Utils ( join )
import Data.Maybe ( catMaybes )
import System.Directory ( doesDirectoryExist, doesFileExist )
import System.FilePath.Posix
	( (</>)
	, replaceExtension
	, splitExtension
	, takeFileName
	, takeExtension )
import Text.Bookbuilder.FilePath ( ls )
import Text.Bookbuilder.Location ( Location )
import qualified Text.Bookbuilder.Pandoc as Pandoc
import Text.Bookbuilder.Template
	( Template
	, TemplateError
	, get
	, isTop )
import qualified Text.Bookbuilder.Template as Template
import Text.Printf ( printf )

styleFile, resourceFile :: String
styleFile = "style.css"
resourceFile = "resources.odt"

data Profile = Profile
	{ _dest       :: String
	, _buildExt   :: String
	, _extra      :: Maybe String
	, _templates  :: [Template]
	} deriving Eq

instance Show Profile where
	show p = printf "%s(%s→%s)-%s" name (_buildExt p) ext (show $ _templates p)
		where (name, ext) = splitExtension $ _dest p



-- | Exported functions

filename :: Profile -> FilePath
filename = _dest

buildFormat :: Profile -> String
buildFormat = takeExtension . _buildExt

load :: FilePath -> IO (Profile, [ProfileWarning])
load path = do
	isDirectory <- doesDirectoryExist path
	if isDirectory
		then runStateT (loadDir path) []
		else return (emptyProfile path, [])

template :: Profile -> Location -> Template
template prof loc = _templates prof `get` loc

lift :: Profile -> FilePath -> String -> String
lift prof leaf = Pandoc.render to . Pandoc.parse from where
	from = Pandoc.readerName leaf
	to = Pandoc.writerName $ _buildExt prof

write :: Profile -> FilePath -> String -> IO ()
write prof dest = write' dest where
	write' = if to == from then writeFile else process prof to from
	from = Pandoc.readerName $ _buildExt prof
	to = Pandoc.writerName $ _dest prof


-- | Load helpers

emptyProfile :: FilePath -> Profile
emptyProfile path = Profile{ _dest = takeFileName path
                           , _buildExt = takeExtension path
                           , _extra = Nothing
                           , _templates = [] }

loadDir :: FilePath -> Warnable IO Profile
loadDir path = do
	let ignored = [styleFile, resourceFile]
	allfiles <- liftIO (ls path)
	let files = filter ((not . (`elem` ignored)) . takeFileName) allfiles
	templates <- catMaybes <$> mapM loadTemplate files
	format <- pickFormat path templates
	hasStyle <- liftIO $ doesFileExist $ path </> styleFile
	hasResources <- liftIO $ doesFileExist $ path </> resourceFile
	let to = Pandoc.writerName path
	let name = takeFileName path
	when (null to) (warn $ UnrecognizedFormat name)
	extra <- extraData path to (hasStyle, hasResources)
	return Profile{ _dest = name
	              , _buildExt = format
	              , _extra = extra
	              , _templates = templates }

loadTemplate :: FilePath -> Warnable IO (Maybe Template)
loadTemplate path = do
	result <- liftIO $ Template.load path
	case result of
		Left err -> warn (BadTemplate err) >> return Nothing
		Right tmpl -> return $ Just tmpl

pickFormat :: String -> [Template] -> Warnable IO String
pickFormat _ [] = return ""
pickFormat path tmpls = do
	let name = takeFileName path
	let allexts = map (takeExtension . Template.name) tmpls
	let exts = sort $ nub $ filter (not . null) allexts
	let tops = filter isTop tmpls
	when (length tops > 1) (warn $ MultipleTops name tops)
	when (length exts > 1) (warn $ NonUniformExtensions name exts)
	let fullname = if null tops then "" else Template.name $ head tops
	let result = if null exts then fullname else head exts
	when (null $ takeExtension result) (warn $ NoExtension path tmpls)
	return $ replaceExtension name result

extraData :: FilePath -> String -> (Bool, Bool) -> Warnable IO (Maybe String)
extraData path "epub" (True, _) = Just <$> liftIO (readFile $ path </> styleFile)
extraData path "odt" (_, True) = return $ Just $ path </> resourceFile
extraData path _ (s, r) = let name = takeFileName path in do
	when s (warn $ IgnoringStyle name)
	when r (warn $ IgnoringResources name)
	return Nothing



-- | Write helpers

process :: Profile -> String -> String -> FilePath -> String -> IO ()
process prof to from path content = Pandoc.write to (_extra prof) path parsed
	where parsed = Pandoc.parse from content



-- | Warnings

type Warnable = StateT [ProfileWarning]

warn :: ProfileWarning -> Warnable IO ()
warn = modify . (:)

data ProfileWarning = BadTemplate TemplateError
                    | UnrecognizedFormat String
                    | MultipleTops String [Template]
					| NonUniformExtensions String [String]
					| NoExtension String [Template]
					| IgnoringStyle String
					| IgnoringResources String

instance Show ProfileWarning where
	show (BadTemplate te) = show te

	show (UnrecognizedFormat name) = "WARNING: " ++
		printf "Unrecognized output format: %s\n" (takeExtension name) ++
		printf "In profile: %s\n" name ++
		"Output format .tex will be used."

	show (MultipleTops name tops) = "WARNING: " ++
		printf "Found multiple top-level templates in profile: %s\n" name ++
		printf "The following templates will always be ignored: %s\n"
			(join ", " $ map Template.name $ tail tops)

	show (NonUniformExtensions name exts) = "WARNING: " ++
		printf "Found multiple extensions in profile: %s\n" name ++
		printf "Using '%s', ignoring %s\n" (head exts)
			(join ", " $ tail exts)

	show (NoExtension name tmpls) = "WARNING: " ++
		printf "Unrecognized extension for profile: %s\n" name ++
		printf "Arising from the files: %s\n"
			(join ", " $ map Template.name tmpls)

	show (IgnoringStyle name) = "WARNING: " ++
		printf "Ignoring style file in %s\n" name

	show (IgnoringResources name) = "WARNING: " ++
		printf "Ignoring resource file in %s\n" name

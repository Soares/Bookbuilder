module Target
    ( Target
    , load
    , render
    , expand
    , theme
    , ext
    , debug
    , write
    , writeTemp
    ) where
import Control.Applicative
import Control.Monad
import Control.Dangerous hiding ( Warning, result )
import Control.Monad.Trans
import Data.Configger ( Config )
import Data.Focus hiding ( parse )
import Data.List
import Data.Maybe
import System.Directory
import System.FilePath
import System.FilePath.Utils hiding ( from )
import Text.Printf
import Text.Pandoc ( Pandoc )
-- Bookbuilder modules
import qualified Target.Config as Config
import qualified Target.Pandoc as Pandoc
import qualified Target.PDF as PDF
import Target.Template ( Template )
import qualified Target.Template as Template

data Target = Target
    { _name      :: String
    , _innerExt  :: String
    , _config    :: Config
    , _templates :: [Template] }
instance Show Target where
    show t = printf "%s(%s→%s)" name from to where
        (name, to) = splitExtension $ _name t
        from = takeExtension $ _innerExt t

-- | Constants

defaultTheme :: String
defaultTheme = "default"


-- | Target loading

load :: Config -> FilePath -> DangerousT IO Target
load conf path = do
    isDirectory <- liftIO $ doesDirectoryExist path
    if isDirectory then loadDir conf path
        else return $ simpleTarget conf path

simpleTarget :: Config -> FilePath -> Target
simpleTarget conf path = Target{ _name      = takeFileName path
                               , _innerExt  = takeExtension path
                               , _config    = conf
                               , _templates = [] }

loadDir :: Config -> FilePath -> DangerousT IO Target
loadDir base dir = do
    files <- filter (not . Config.isSpecial) <$> liftIO (ls dir)
    templates <- catMaybes <$> mapM loadTemplate files
    format <- pickFormat dir templates
    let name = takeFileName dir
    let to = Pandoc.writerName name
    when (null to) (warn $ UnrecognizedFormat name)
    conf <- Config.merge base dir to
    return Target{ _name      = name
                 , _innerExt  = format
                 , _config    = conf
                 , _templates = templates }

loadTemplate :: FilePath -> DangerousT IO (Maybe Template)
loadTemplate path = do
    result <- liftIO $ Template.load path
    case result of
        Left err -> warn (BadTemplate err) >> return Nothing
        Right tmpl -> return $ Just tmpl

pickFormat :: String -> [Template] -> DangerousT IO String
pickFormat _ [] = return ""
pickFormat path tmpls = do
    let name = takeFileName path
    let allexts = map (takeExtension . Template.name) tmpls
    let exts = sort $ nub $ filter (not . null) allexts
    let tops = filter Template.isTop tmpls
    when (length tops > 1) (warn $ MultipleTops name tops)
    when (length exts > 1) (warn $ NonUniformExtensions name exts)
    let fullname = if null tops then "" else Template.name $ head tops
    let result = if null exts then fullname else head exts
    when (null $ takeExtension result) (warn $ NoExtension path tmpls)
    return $ takeExtension result

-- | Target manipulation

debug :: Target -> Bool
debug = Config.debug . _config

theme :: Target -> Maybe String
theme t = let name = takeFileName (_name t) in
    if name == defaultTheme then Nothing else Just name

ext :: Target -> String
ext = takeExtension . _name


-- | Section affectors

expand :: Target -> Focus -> [(String, String)] -> String
expand t foc extra = Template.render vars tmpl where
    tmpl = Template.get (_templates t) foc
    vars = extra ++ Config.vars (_config t)

render :: Target -> Pandoc -> String
render t doc = Pandoc.render to doc where
    to = Pandoc.writerName $ _innerExt t


-- | Target output

write :: FilePath -> String -> Target -> IO ()
write dest text target = output text where
    output | (to, from) == ("pdf", "latex") = PDF.outputLaTeX conf dest
           | to == from = writeFile dest
           | otherwise = Pandoc.write to conf dest . parse
    conf = _config target
    from = Pandoc.readerName $ _innerExt target
    to = Pandoc.writerName $ _name target
    parse = Pandoc.parse from

writeTemp :: FilePath -> String -> Target -> IO ()
writeTemp path text target = writeFile (path <.> ext target) text


data Warning = BadTemplate Template.Error
             | UnrecognizedFormat String
             | MultipleTops String [Template]
			 | NonUniformExtensions String [String]
			 | NoExtension String [Template]
			 | IgnoringStyle String
			 | IgnoringResources String

instance Show Warning where
	show (BadTemplate te) = show te

	show (UnrecognizedFormat name) = "WARNING: " ++
		printf "Unrecognized output format: %s\n" (takeExtension name) ++
		printf "\tIn profile: %s\n" name ++
		"\tOutput format .tex will be used."

	show (MultipleTops name tops) = "WARNING: " ++
		printf "Found multiple top-level templates in profile: %s\n" name ++
		printf "\tThe following templates will always be ignored: %s\n"
			(intercalate ", " $ map Template.name $ tail tops)

	show (NonUniformExtensions name exts) = "WARNING: " ++
		printf "Found multiple extensions in profile: %s\n" name ++
		printf "\tUsing '%s', ignoring %s\n" (head exts)
			(intercalate ", " $ tail exts)

	show (NoExtension name tmpls) = "WARNING: " ++
		printf "Unrecognized extension for profile: %s\n" name ++
		printf "\tArising from the files: %s\n"
			(intercalate ", " $ map Template.name tmpls)

	show (IgnoringStyle name) = "WARNING: " ++
		printf "Ignoring style file in %s\n" name

	show (IgnoringResources name) = "WARNING: " ++
		printf "Ignoring resource file in %s\n" name

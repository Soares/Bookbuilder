module Target.Config
    ( isSpecial
    , isConfig
    , load
    , merge
    , vars
    , values
    , setValue
    , option
    , setOption
    , set
    , debug
    , setDebug
    , style
    , metadata
    , resources
    , latex
    , parseBool
    ) where
import Control.Monad
import Control.Monad.Trans
import Control.Monad.Loops
import Control.Dangerous hiding ( Warning )
import Data.Char
import Data.List.Utils hiding ( merge )
import qualified Data.Configger as Configger
import Data.Configger ( Config )
import Data.Maybe
import System.Directory
import System.FilePath
import Text.Printf

-- | Constants

configFile :: String
configFile = "config"

variableSection, optionSection, localSection :: String
variableSection = "VARIABLES"
optionSection = "OPTIONS"
localSection = "LOCAL"

styleFile, resourceFile, metadataFile :: String
styleFile = "style.css"
resourceFile = "resources.odt"
metadataFile = "metadata.xml"

styleOption, resourceOption, metadataOption :: String
styleOption = "style"
resourceOption = "resources"
metadataOption = "metadata"

latexOption, debugOption :: String
latexOption = "latex"
debugOption = "debug"


-- | Config file loading and manipulation

isCover :: String -> Bool
isCover f = startswith "cover-image." name || startswith "cover." name
    where name = takeFileName f

isSpecial :: FilePath -> Bool
isSpecial f = isCover f || (takeFileName f) `elem` special where
    special = [configFile, styleFile, resourceFile, metadataFile]

isConfig :: FilePath -> Bool
isConfig = (== configFile) . takeFileName

raw :: FilePath -> DangerousT IO Config
raw dir = do
    let cfile = dir </> configFile
    hasConfig <- liftIO $ doesFileExist cfile
    if hasConfig then Configger.load variableSection cfile else return []

load :: FilePath -> DangerousT IO Config
load path = do
    let sfile = path </> styleFile
    let rfile = path </> resourceFile
    let mfile = path </> metadataFile
    hasStyle <- liftIO $ doesFileExist sfile
    hasResources <- liftIO $ doesFileExist rfile
    hasMetadata <- liftIO $ doesFileExist mfile
    basic <- raw path
    let setStyle c = do
        css <- liftIO $ readFile sfile
        return $ setOption styleOption css c
    let setMetadata c = do
        meta <- liftIO $ readFile mfile
        return $ setOption metadataOption meta c
    let setResources = return . setOption resourceOption rfile
    let setters = [setStyle | hasStyle] ++
                  [setResources | hasResources] ++
                  [setMetadata | hasMetadata]
    conf <- concatM setters basic
    let locals = Configger.items localSection conf
    let mapped = map (\(x, y) -> (x, path </> y)) locals
    return $ Configger.mergeSection (variableSection, mapped) conf

merge :: Config -> FilePath -> String -> DangerousT IO Config
merge conf dir fmt = let name = takeFileName dir in do
    new <- load dir
    when (isJust (option styleOption new) && fmt /= "epub")
        (warn $ IgnoringStyle name)
    when (isJust (option metadataOption new) && fmt /= "epub")
        (warn $ IgnoringMetadata name)
    when (isJust (option resourceOption new) && fmt /= "odt")
        (warn $ IgnoringResources name)
    return $ Configger.merge new conf

vars :: Config -> [(String, String)]
vars = Configger.items variableSection

values :: String -> Config -> [String]
values str conf = map snd $ filter ((str ==) . fst) (vars conf)

setValue :: String -> String -> Config -> Config
setValue = Configger.set variableSection

option :: String -> Config -> Maybe String
option = Configger.get optionSection

setOption :: String -> String -> Config -> Config
setOption = Configger.set optionSection

set :: String -> String -> Config -> Config
set = Configger.set variableSection

debug :: Config -> Bool
debug conf = maybe False parseBool (option debugOption conf)

setDebug :: Config -> Config
setDebug = Configger.set optionSection debugOption (show True)

style :: Config -> Maybe String
style = option styleOption

metadata :: Config -> Maybe String
metadata = option metadataOption

resources :: Config -> Maybe String
resources = option resourceOption

latex :: Config -> Maybe String
latex = option latexOption

parseBool :: String -> Bool
parseBool = (`elem` ["true", "yes", "on", "1"]) . map toLower

data Warning = IgnoringStyle String
			 | IgnoringResources String
             | IgnoringMetadata String

instance Show Warning where
	show (IgnoringStyle name) = "WARNING: " ++
		printf "Ignoring style file in %s\n" name

	show (IgnoringResources name) = "WARNING: " ++
		printf "Ignoring resource file in %s\n" name

	show (IgnoringMetadata name) = "WARNING: " ++
		printf "Ignoring metadata file in %s\n" name

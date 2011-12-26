module Target.Config
    ( isSpecial
    , isConfig
    , load
    , merge
    , vars
    , option
    , setOption
    , set
    , debug
    , setDebug
    , style
    , resources
    , latex
    ) where
import Control.Monad
import Control.Monad.Trans
import Control.Monad.Loops
import Control.Dangerous hiding ( Warning )
import qualified Data.Configger as Config
import Data.Configger ( Config )
import Data.Maybe
import System.Directory
import System.FilePath
import Text.Printf

-- | Constants

configFile :: String
configFile = "config"

variableSection, optionSection :: String
variableSection = "VARIABLES"
optionSection = "OPTIONS"

styleFile, resourceFile :: String
styleFile = "style.css"
resourceFile = "resources.odt"

styleOption, resourceOption, latexOption, debugOption :: String
styleOption = "style"
resourceOption = "resources"
latexOption = "latex"
debugOption = "debug"


-- | Config file loading and manipulation

isSpecial :: FilePath -> Bool
isSpecial = flip elem [configFile, styleFile, resourceFile] . takeFileName

isConfig :: FilePath -> Bool
isConfig = (== configFile) . takeFileName

load :: FilePath -> DangerousT IO Config
load path = do
    let cfile = path </> configFile
    let sfile = path </> styleFile
    let rfile = path </> resourceFile
    hasConfig <- liftIO $ doesFileExist cfile
    hasStyle <- liftIO $ doesFileExist sfile
    hasResources <- liftIO $ doesFileExist rfile
    conf <- if hasConfig then Config.load variableSection cfile else return []
    let setStyle c = do
        css <- liftIO $ readFile sfile
        return $ setOption styleOption css c
    let setResources = return . setOption resourceOption rfile
    let setters = [setStyle | hasStyle] ++ [setResources | hasResources]
    concatM setters conf

merge :: Config -> FilePath -> String -> DangerousT IO Config
merge conf dir fmt = let name = takeFileName dir in do
    new <- Config.load variableSection dir
    when (isJust (option styleOption new) && fmt /= "epub")
        (warn $ IgnoringStyle name)
    when (isJust (option resourceOption new) && fmt /= "odt")
        (warn $ IgnoringResources name)
    return $ Config.merge new conf

vars :: Config -> [(String, String)]
vars = Config.items variableSection

option :: String -> Config -> Maybe String
option = Config.get optionSection

setOption :: String -> String -> Config -> Config
setOption = Config.set optionSection

set :: String -> String -> Config -> Config
set = Config.set variableSection

debug :: Config -> Bool
debug conf = maybe False read (option debugOption conf)

setDebug :: Bool -> Config -> Config
setDebug = Config.set optionSection debugOption . show

style :: Config -> Maybe String
style = option styleOption

resources :: Config -> Maybe String
resources = option resourceOption

latex :: Config -> Maybe String
latex = option latexOption

data Warning = IgnoringStyle String
			 | IgnoringResources String

instance Show Warning where
	show (IgnoringStyle name) = "WARNING: " ++
		printf "Ignoring style file in %s\n" name

	show (IgnoringResources name) = "WARNING: " ++
		printf "Ignoring resource file in %s\n" name

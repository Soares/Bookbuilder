module Text.Bookbuilder.Config
    ( Options(..)
    , Config
    , Configged
    , configure
    , root
    , src
    , dest
    , profiles
    , Range
    , range
    , isInRange
    , context
    ) where

import Prelude hiding ( error )
import Control.Arrow ( (***), first, second )
import Control.Monad ( unless )
import Control.Monad.Loops ( concatM )
import Control.Monad.Reader ( ReaderT )
import Control.Monad.State ( StateT, runStateT, modify )
import Control.Monad.Trans ( liftIO )
import Data.Functor ( (<$>) )
import Data.Maybe ( fromMaybe, mapMaybe )
import Data.List.Utils ( join )
import System.Directory ( doesDirectoryExist )
import System.FilePath.Posix
    ( (</>), (<.>)
    , splitPath
    , splitExtension )
import Text.Bookbuilder.FilePath
    ( ls
    , exists
    , ancestorWith
    , canonicalizeHere
    , from )
import Text.Bookbuilder.Location
    ( Location
    , Range
    , fromString
    , toList
    , focus
    , nowhere )
import Text.Bookbuilder.Profile ( Profile, ProfileWarning )
import qualified Text.Bookbuilder.Profile as Profile
import Text.Printf ( printf )


data Options = Options
    { optRoot       :: FilePath
    , optSourceDir  :: FilePath
    , optProfileDir :: FilePath
    , optBuildDir   :: FilePath
    , optStart      :: Maybe Location
    , optEnd        :: Maybe Location
    , optDetect     :: Bool
    , optVars       :: [(String, String)]
    , optDebug      :: Bool
    , optHelp       :: Bool
    } deriving Show

data Config = Config
    { _root     :: FilePath
    , _src      :: FilePath
    , _build    :: FilePath
    , _profiles :: [Profile]
    , _vars     :: [(String, String)]
    , _range    :: Range
    } deriving Show

type Configged = ReaderT Config

defaultConfig :: Config
defaultConfig = Config "" "" "" [] [] (nowhere, nowhere)

root :: Config -> FilePath
root = _root

src :: Config -> FilePath
src = _src

dest :: Profile -> Config -> FilePath
dest prof conf = _build conf </> filename where
    (name, ext) = splitExtension $ Profile.filename prof
    (start, end) = (toList *** toList) (_range conf)
    filename = name' <.> ext
    name' = join "-" (name : parts start end)
    parts [] [] = []
    parts lo hi = if lo == hi then [sep lo] else [sep lo, sep hi]
    sep = join "_" . map show

profiles :: Config -> [Profile]
profiles = _profiles

range :: Config -> (Location, Location)
range = _range

context :: Config -> [(String, String)]
context = _vars

isInRange :: Location -> Config -> Bool
isInRange loc conf = loc >= lo && loc <= hi where (lo, hi) = range conf

configure :: Options -> IO (Either [ConfigError] Config, [ConfigWarning])
configure opts = do
    start <- canonicalizeHere $ optRoot opts
    let opts' = opts{ optRoot = start }
    let setters = [setRoot, setSrc, setBuild, setProfiles, setRange, setVars]
    let make = concatM (map ($ opts') setters) defaultConfig
    (config, (errs, warnings)) <- runStateT make ([], [])
    return (if null errs then Right config else Left errs, warnings)



-- | Root discovery

setRoot :: Options -> Config -> Dangerously IO Config
setRoot opts conf | optDetect opts = set <$> detect start requisites
                  | otherwise = return $ set start where
    requisites = map ($ opts) [optSourceDir, optProfileDir, optBuildDir]
    start = optRoot opts
    set r = conf{ _root = r }

detect :: FilePath -> [FilePath] -> Dangerously IO FilePath
detect path [] = error (NoSource path) >> return path
detect path children = let err = error (CantDetect path children) in do
    result <- liftIO $ path `ancestorWith` children
    case result of
        Nothing -> err >> return path
        Just found -> return found



-- Source directory assurance

setSrc :: Options -> Config -> Dangerously IO Config
setSrc opts conf = let path = root conf </> optSourceDir opts in do
    there <- liftIO $ exists path
    unless there (error $ NoSource path)
    return conf{ _src = path }



-- Build directory assurance

setBuild :: Options -> Config -> Dangerously IO Config
setBuild opts conf = let path = root conf </> optBuildDir opts in do
    there <- liftIO $ doesDirectoryExist path
    unless there (error $ NoBuild path)
    return conf{ _build = path }



-- Profile loading

setProfiles :: Options -> Config -> Dangerously IO Config
setProfiles opts conf = let path = root conf </> optProfileDir opts in do
    there <- liftIO $ doesDirectoryExist path
    if there then do
        files <- liftIO $ ls path
        (profs, warnings) <- merge <$> liftIO (mapM Profile.load files)
        unless (null warnings) (warn $ ProfileWarnings warnings)
        return conf{ _profiles = profs }
        else error (NoProfiles path) >> return conf
    where merge = foldr (\(p, ws) (ps, wss) -> (p:ps, ws++wss)) ([],[])



-- | Range discovery

setRange :: Options -> Config -> Dangerously IO Config
setRange opts conf | optDetect opts = do
    let path = optRoot opts `from` src conf
    let path' = if path == optRoot opts then "" else path
    let parts = splitPath path'
    let locations = mapMaybe fromString parts
    let location = foldr focus nowhere locations
    return $ use $ if null path' then nowhere else location
                   | otherwise = return $ use nowhere
    where use loc = let
        lo = fromMaybe loc $ optStart opts
        hi = fromMaybe loc $ optEnd opts
        in conf{ _range = (lo, hi) }



-- | Variable loading

setVars :: Options -> Config -> Dangerously IO Config
setVars opts conf = return conf{ _vars = optVars opts }



-- Error and Warning handling

type Dangerously = StateT ([ConfigError], [ConfigWarning])

error :: ConfigError -> Dangerously IO ()
error e = modify $ first ((:) e)

warn :: ConfigWarning -> Dangerously IO ()
warn w = modify $ second ((:) w)

data ConfigError = NoRoot FilePath
                 | CantDetect FilePath [FilePath]
                 | NoSource FilePath
                 | NoBuild FilePath
                 | NoProfiles FilePath

instance Show ConfigError where
    show (NoRoot path) =
        printf "ERROR: book root '%s' not found\n" path

    show (CantDetect path dirs) =
        "ERROR: can't detect book root.\n" ++
        printf "\tSearched from: %s\n" path ++
        printf "\tLooked for directories: %s\n" (join ", " dirs)

    show (NoSource path) =
        printf "ERROR: book source '%s' not found\n" path

    show (NoBuild path) =
        printf "ERROR: build directory '%s' not found\n" path

    show (NoProfiles path) =
        printf "ERROR: profile directory '%s' not found\n" path


data ConfigWarning = ProfileWarnings [ProfileWarning]

instance Show ConfigWarning where
    show (ProfileWarnings ws) = join "\n" $ map show ws

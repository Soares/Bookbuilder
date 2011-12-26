module Book ( Book, load, compile ) where
import Control.Applicative
import Control.Arrow
import Control.Dangerous hiding ( Exit, Warning, execute )
import Control.Monad
import Control.Monad.Trans
import Data.List
import Data.Focus
import Data.Maybe
import Data.Scope
import System.FilePath
import System.FilePath.Utils
import Text.Printf
import Target hiding ( load )
import qualified Path
import qualified Target
import qualified Target.Config as Config
import qualified Section
import Options

data Book = Book
    { _title    :: String
    , _scope    :: Scope
    , _root     :: Section.Section
    , _build    :: FilePath
    , _targets  :: [Target] }


-- | Loading

load :: Options -> DangerousT IO Book
load opts = do
    -- Canonicalize the root, in case we have a weird relative path
    start <- liftIO $ canonicalizeHere $ optRoot opts

    -- Detect the top if they are currently within the book
    let dirs = map ($ opts) [optSourceDir, optTargetDir, optBuildDir]
    top <- if optDetect opts
        then liftIO (start `ancestorWith` dirs) >>=
            maybe (throw $ CantDetect start dirs) return
        else return start

    -- Ensure that the other directories exist
    let ensure dir err = let path = top </> dir opts in do
        there <- liftIO $ exists path
        unless there $ throw (err path)
        return path
    srcDir <- ensure optSourceDir NoSource
    buildDir <- ensure optBuildDir NoBuild
    targetDir <- ensure optTargetDir NoTarget

    -- Load the data
    let title = Path.title $ takeFileName top
    let scope = getScope start srcDir opts
    root <- liftIO $ Section.load srcDir "" scope

    -- Load the targets
    conf <- Config.load targetDir
    let conf' = if optDebug opts then Config.setDebug conf else conf
    paths <- filter (not . Config.isSpecial) <$> liftIO (ls targetDir)
    targets <- mapM (Target.load conf) paths

    -- Build the book
    return Book{ _title = title
               , _scope = scope
               , _root = root
               , _build = buildDir
               , _targets = targets }


getScope :: FilePath -> FilePath -> Options -> Scope
getScope start src opts | optDetect opts = use $ let
    path = start `from` src
    path' = if path == start then "" else path
    parts = splitPath path'
    locations = mapMaybe fromString parts
    location = foldr focus unfocused locations
    in if null path' then unfocused else location
                        | otherwise = use unfocused
    where use = fromTuple . (let get = (flip fromMaybe . ($ opts))
                             in get optStart &&& get optEnd)


-- | Writing

compile :: Book -> IO ()
compile book = mapM_ (execute book) (_targets book) where

execute :: Book -> Target -> IO ()
execute book target = do
    let d = dest book target
    let t = text book target
    putStrLn $ statusMsg d target
    when (debug target) $ writeTemp d t target
    write d t target

text :: Book -> Target -> String
text book target = Section.flatten (_title book) (_root book)
    (render target) (expand target)

dest :: Book -> Target -> FilePath
dest book target = let
    title = _title book
    scope = _scope book
    suffix = [show scope | not $ isEverywhere scope]
    name = fromMaybe title (theme target)
    in intercalate "-" (name : suffix)

statusMsg :: FilePath -> Target -> String
statusMsg path target = printf "%s [%s]" path (show target)


-- | Errors

data Error = NoSource FilePath
           | NoTarget FilePath
           | NoBuild FilePath
           | CantDetect FilePath [FilePath]
instance Show Error where
    show (NoSource path) = printf "source directory '%s' not found\n" path
    show (NoTarget path) = printf "target directory '%s' not found\n" path
    show (NoBuild path) = printf "build directory '%s' not found\n" path
    show (CantDetect path dirs) = "can't detect book root.\n" ++
        printf "\tSearched from: %s\n" path ++
        printf "\tLooked for directories: %s\n" (intercalate ", " dirs)

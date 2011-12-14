module Context where

import Utils (makeMeta, lastChanged, listDir)
import Control.Monad.Reader (Reader, ReaderT)
import System.Directory (doesFileExist)
import System.FilePath.Posix ( combine
                             , takeBaseName
                             , dropTrailingPathSeparator )
import Text.Pandoc ( Pandoc(Pandoc)
                   , Meta(Meta)
                   , WriterOptions(..)
                   , defaultWriterOptions )

import Format (Format)
import qualified Format
import qualified Config

-- Context
data Context = Context { root :: FilePath
                       , location :: [FilePath] }
             deriving (Eq, Ord, Read, Show)

type Contexted = Reader Context
type Contextual = ReaderT Context

name :: Context -> String
name = takeBaseName . dropTrailingPathSeparator . root

place :: FilePath -> Context -> FilePath
place dir c = foldl combine base (reverse $ location c) where
    base = combine (root c) dir

-- Todo: rename this
src :: Context -> FilePath
src = place Config.srcDir

-- Todo: rename this
build :: Context -> FilePath
build = place Config.buildDir

isFile :: Context -> IO Bool
isFile = doesFileExist . src

-- Todo: error unless doesFileExist $ src c
loader :: Context -> IO String
loader = readFile . src

processor :: Context -> String -> Pandoc
processor = const Config.parser

outputerFor :: Context -> Pandoc -> Format -> IO ()
outputerFor c pandoc format = do
    style <- loadStyle format
    template <- loadTemplate format
    let options = (build c, writerOptions{writerTemplate=template}, style)
    Format.outputer options format pandoc

outputer :: Context -> Pandoc -> IO ()
outputer c doc = mapM_ (return . outputerFor c doc) Config.formats

meta :: Context -> Meta
meta c = makeMeta (Config.makeTitle (name c) (location c)) Config.authors

isDirty :: Context -> IO Bool
isDirty c = do
    touchTime <- lastChanged $ src c
    let dest = Format.path (build c)
    writeTimes <- mapM (lastChanged . dest) Config.formats
    return $ touchTime > maximum writeTimes

-- Todo: error unless doesDirExist $ src c
fragments :: Context -> IO [FilePath]
fragments = listDir . src

joiner :: Context -> Pandoc -> Pandoc -> Pandoc
joiner c (Pandoc meta b1) (Pandoc _ b2) = Pandoc meta (b1 `with` b2) where
    -- Todo: wtf? Can't place header. Bitch.
    with = Config.combine (location c)

descend :: Context -> FilePath -> Context
descend c f = c{location=f:location c}

-- Todo: these
writerOptions = defaultWriterOptions{ writerStandalone=True
                                    , writerChapters=True }

loadStyle :: Format -> IO (Maybe String)
loadStyle f = return Nothing

loadTemplate :: Format -> IO String
loadTemplate f = return ""

module Utils where

import Control.Monad (filterM)
import Data.String.Utils (join)
import System.Directory (getDirectoryContents)
import System.Posix.Types (EpochTime)
import System.Posix.Files (getFileStatus, modificationTime)
import Text.Pandoc ( Pandoc(Pandoc)
                   , Meta
                   , readMarkdown
                   , defaultParserState )

-- General Utilities
leadingIndexCharacters = "0123456789.-"
stripIndex :: String -> String
stripIndex = dropWhile (flip elem leadingIndexCharacters)

-- File Utilities
dotFile :: FilePath -> Bool
dotFile (x:xs) = x == '.'

listDir :: FilePath -> IO [FilePath]
listDir f = do
    files <- getDirectoryContents f
    filterM (return . not . dotFile) files

lastChanged :: FilePath -> IO EpochTime
lastChanged = fmap modificationTime . getFileStatus

-- Pandoc Utilities
withMeta :: Pandoc -> Meta -> Pandoc
(Pandoc _ bs) `withMeta` m = Pandoc m bs

getMeta :: Pandoc -> Meta
getMeta (Pandoc m _) = m

makeMeta :: String -> [String] -> Meta
makeMeta title authors = meta where
    head = "% " ++ title ++ "\n% " ++ ("\n  " `join` authors)
    (Pandoc meta _) = readMarkdown defaultParserState head

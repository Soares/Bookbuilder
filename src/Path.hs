module Path ( title, location ) where
import Data.Focus hiding ( split )
import Data.Maybe
import Data.List.Split
import System.FilePath

title :: FilePath -> String
title = deCamel . strip . dropExtension . takeFileName where
    deCamel = unwords . split (startsWithOneOf ['A'..'Z'])

location :: FilePath -> Focus
location = fromMaybe unfocused . fromString . takeFileName

module Target.Pandoc where

import Codec.Binary.UTF8.String ( encodeString )
import Data.Char ( toLower )
import Data.Configger ( Config )
import Data.Maybe ( fromMaybe )
import System.FilePath.Posix ( takeExtension )
import Text.Pandoc ( Pandoc
                   , WriterOptions(..)
                   , ParserState(..)
                   , readers
                   , writers
                   , defaultParserState
                   , defaultWriterOptions
                   , readMarkdown
                   , writeEPUB
                   , writeODT
                   , writeLaTeX )
import qualified Data.ByteString.Lazy as B
import qualified Target.PDF as PDF
import qualified Target.Config as Config

defaultReader :: ParserState -> String -> Pandoc
defaultReader = readMarkdown

defaultWriter :: WriterOptions -> Pandoc -> String
defaultWriter = writeLaTeX

parse :: String -> String -> Pandoc
parse format = r defaultParserState{ stateSmart = True } where
    r = fromMaybe defaultReader (lookup format readers)

render :: String -> Pandoc -> String
render format = w defaultWriterOptions where
    w = fromMaybe defaultWriter (lookup format writers)

write :: String -> Config -> FilePath -> Pandoc -> IO ()
write format conf dest doc = case lookup format writers of
    Nothing | format == "pdf" ->
        PDF.outputLaTeX conf dest $ writeLaTeX opts doc
    Nothing | format == "epub" ->
        writeEPUB (Config.style conf) epubOpts doc
        >>= B.writeFile (encodeString dest)
    Nothing | format == "odt" ->
        writeODT (Config.resources conf) opts doc
        >>= B.writeFile (encodeString dest)
    Nothing | format == "-" -> putStr $ defaultWriter opts doc
    Nothing -> writeFile dest $ defaultWriter opts doc
    Just r -> writeFile dest $ r opts doc
    where opts = defaultWriterOptions
          epubOpts = opts{ writerEPUBMetadata = metadata }
          metadata = fromMaybe "" (Config.metadata conf)

readerName :: FilePath -> String
readerName x = case takeExtension (map toLower x) of
    ".xhtml"    -> "html"
    ".html"     -> "html"
    ".htm"      -> "html"
    ".tex"      -> "latex"
    ".latex"    -> "latex"
    ".ltx"      -> "latex"
    ".rst"      -> "rst"
    ".lhs"      -> "markdown+lhs"
    ".textile"  -> "textile"
    ".native"   -> "native"
    ".json"     -> "json"
    ".markdown" -> "markdown"
    ".md"       -> "markdown"
    ".text"      -> "markdown"
    ".txt"       -> "markdown"
    _            -> ""

writerName :: FilePath -> String
writerName x = case takeExtension (map toLower x) of
    ".tex"      -> "latex"
    ".latex"    -> "latex"
    ".ltx"      -> "latex"
    ".context"  -> "context"
    ".ctx"      -> "context"
    ".rtf"      -> "rtf"
    ".rst"      -> "rst"
    ".s5"       -> "s5"
    ".native"   -> "native"
    ".json"     -> "json"
    ".txt"      -> "markdown"
    ".text"     -> "markdown"
    ".md"       -> "markdown"
    ".markdown" -> "markdown"
    ".textile"  -> "textile"
    ".lhs"      -> "markdown+lhs"
    ".texi"     -> "texinfo"
    ".texinfo"  -> "texinfo"
    ".db"       -> "docbook"
    ".odt"      -> "odt"
    ".epub"     -> "epub"
    ".org"      -> "org"
    ".pdf"      -> "pdf"
    ['.',y] | y `elem` ['1'..'9'] -> "man"
    ".-"        -> "-"
    _           -> ""

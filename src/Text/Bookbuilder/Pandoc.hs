module Text.Bookbuilder.Pandoc where

import Codec.Binary.UTF8.String ( encodeString )
import Data.Char ( toLower )
import Data.Maybe ( fromMaybe )
import System.FilePath.Posix ( takeExtension )
import Text.Pandoc ( Pandoc
                   , WriterOptions
                   , ParserState
                   , readers
                   , writers
                   , defaultParserState
                   , defaultWriterOptions
                   , readMarkdown
                   , writeEPUB
                   , writeODT
                   , writeLaTeX )
import qualified Data.ByteString.Lazy as B

defaultReader :: ParserState -> String -> Pandoc
defaultReader = readMarkdown

defaultWriter :: WriterOptions -> Pandoc -> String
defaultWriter = writeLaTeX

parse :: String -> String -> Pandoc
parse format = r defaultParserState where
	r = fromMaybe defaultReader (lookup format readers)

render :: String -> Pandoc -> String
render format = w defaultWriterOptions where
	w = fromMaybe defaultWriter (lookup format writers)

write :: String -> Maybe String -> FilePath -> Pandoc -> IO ()
write format extra dest content = case lookup format writers of
	Nothing | format == "epub" ->
		writeEPUB extra ops content
		>>= B.writeFile (encodeString dest)
	Nothing | format == "odt" ->
		writeODT extra ops content
		>>= B.writeFile (encodeString dest)
	Nothing | format == "-" -> putStr $ guess ops content
	Nothing -> writeFile dest $ guess ops content
	Just r -> writeFile dest $ r ops content
	where (guess, ops) = (defaultWriter, defaultWriterOptions)

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
	['.',y] | y `elem` ['1'..'9'] -> "man"
	".-"        -> "-"
	_           -> ""

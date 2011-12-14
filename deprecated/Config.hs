module Config where
import Format (toLaTeX, toEPUB)
import Text.Pandoc ( Pandoc
                   , Block
                   , readMarkdown
                   , defaultParserState )
import Utils (stripIndex)

srcDir = "src"
buildDir = "build"
authors = ["Nate So8res"]
formats = [toLaTeX, toEPUB]
reader = readMarkdown defaultParserState

-- Todo: remove reader
parser :: String -> Pandoc
parser = readMarkdown defaultParserState

-- Todo: these
combine :: Int -> [Block] -> [Block] -> [Block]
combine n = (++)

makeTitle :: String -> [FilePath] -> String
makeTitle t [] = stripIndex t
makeTitle t (pos:_) = stripIndex pos

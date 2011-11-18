module Config where
import System.FilePath.Posix (combine)
import Control.Monad.Reader (Reader, ReaderT)
import Text.Pandoc (
    Pandoc(Pandoc), Meta(docTitle, docAuthors), Block, Inline,
    writeLaTeX, defaultWriterOptions,
    readMarkdown, defaultParserState)

-- Todo: doc this section
getMeta :: Pandoc -> Meta
getMeta (Pandoc m _) = m

pandocTitle :: String -> [Inline]
pandocTitle = docTitle . getMeta . readMarkdown defaultParserState . ("% " ++)

dropPast :: (Eq a) => a -> [a] -> [a]
dropPast _ [] = []
dropPast x (a:as) | x == a = as
                  | otherwise = dropPast x as

data Format = Format { ext :: String, writer :: (Pandoc -> String) }

dest :: FilePath -> Format -> FilePath
dest p f = p ++ "." ++ (ext f)

outputter :: FilePath -> Format -> Pandoc -> IO ()
outputter p f = writeFile p . writer f

-- Todo: add EPUB
latex = Format "tex" (writeLaTeX defaultWriterOptions)

data Config = Config { src :: FilePath
                     , config :: FilePath
                     , build :: FilePath
                     , formats :: [Format] }

path :: Config -> FilePath -> [FilePath] -> FilePath
path conf root loc = foldl combine root $ (src conf):(reverse loc)

rootDest :: Config -> FilePath -> [FilePath] -> FilePath
rootDest conf root loc = foldl combine root $ (build conf):(reverse loc)

dests :: Config -> FilePath -> [FilePath] -> [FilePath]
dests conf root loc = [dest base format | format <- formats conf] where
    base = rootDest conf root loc

parser :: Config -> String -> Pandoc
-- Todo: we should actually parse things!
parser conf = readMarkdown defaultParserState

writers :: Config -> FilePath -> [FilePath] -> [Pandoc -> IO ()]
writers conf root loc = map (outputter base) (formats conf) where
    base = rootDest conf root loc

makeTitle :: Config -> [FilePath] -> Bool -> [Inline]
makeTitle conf (f:fs) isLeaf | isLeaf = pandocTitle $ dropPast '-' f
                             | otherwise = pandocTitle $ f

-- Todo: we should provide the author file, not parse it
-- Todo: parse the whole damn meta, damnit. Toss in the title if it's absent.
authors :: Config -> FilePath -> IO [[Inline]]
authors conf root = do
    text <- readFile $ combine root (config conf)
    let pandoc = readMarkdown defaultParserState text
    return $ docAuthors $ getMeta pandoc

combiner :: Config -> [FilePath] -> [Block] -> [Block] -> [Block]
-- Todo: actually combine things
combiner conf loc = (++)

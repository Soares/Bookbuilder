module Build where

import Control.Monad.Reader
import Text.Pandoc

data Document = Chapter Title Pandoc
              | Strand Title [Chapter]
							| Novel Title [Strand]

flatten :: Document -> Pandoc
flatten (Chapter t p) = p `withTitle` t
flatten (Strand t cs) = (joinChapters cs) `withTitle` t
flatten (Novel t ss) = (joinStrands ss) `withTitle` t

loadNovel :: Config -> Title -> IO Document
loadNovel conf path = do
	let name = novelTitle conf path
	strandNames <- strandsIn conf
	strands <- mapM (loadStrand conf) strandNames
	return $ Novel name strands

loadStrand :: Config -> Title -> Title -> IO Document
loadStrand conf title path = do
	let name = strandTitle conf title path
	chapterNames <- chaptersIn conf title
	chapters <- mapM (loadChapter conf) chapterNames
	return $ Strand name chapters

loadChapter :: Config -> Title -> Title -> Title -> IO Document
loadChapter conf title strand path = do
	let name = chapterTitle conf title strand path
	return $ Chapter name doc
	

main :: IO ()
main = do
	let conf = Config "/home/nate/Dropbox/Project/Books/Resurgence"
	novel <- runReader loadNovel conf



{-
data Mode = Markdown | LaTeX | EPUB

readDoc :: Mode -> String -> Pandoc
readDoc Markdown = readMarkdown defaultParserState
-- readDoc EPUB = readMarkdown defaultParserState
readDoc LaTeX = readLaTeX defaultParserState

writeDoc :: Mode -> Pandoc -> String
writeDoc Markdown = writeMarkdown defaultWriterOptions
-- writeDoc EPUB = writeEPUB defaultWriterOptions
writeDoc LaTeX = writeLaTeX defaultWriterOptions

data Config = Config FilePath
type Configed = Reader Config
type ConfigedIO = ReaderT Config IO

import Types (Name)

data Config = Config FilePath
type Configed = Reader Config
type IOConfiged = ReaderT Config IO

-- Why do we touch the document? Ever?
data Mode = LaTeX | Epub

complete :: (Document a) => Config -> a -> Pandoc
writerOptions :: Config -> WriterOptions
readerOptions :: Config -> ReaderOptions
destination :: Config -> Mode -> FilePath
name :: Config -> Name
childrenOf :: Config -> [Name]



--- Below is useless

strandsIn :: Config -> IO [Config]

strandsIn :: Config -> IO [Config]
strandsIn conf = do
  let root = location conf
  let dotstrands = combine root $ strandOrder conf
  names <- readLines dotstrands
  let isThere = doesFileExist . (combine root)
  existing <- filterM isThere names
  return $ map (strandConfig conf) existing

strandConfig :: Config -> Name -> Config

chaptersIn :: Config -> Name -> IO [Config]
chaptersIn conf strand = do
  let root = location conf
  names <- readLines root
  let noNum = (drop 3)

metaFor :: (Document a) => a -> Meta
-}

module Document where

import Control.Monad.Reader (asks, local)

import Config (Configed, IOConfiged)

type Chapter = IO Pandoc
type Strand = IO [Chapter]
type Novel = IO [[Strand]]

type Name = String
data Chapter = Chapter Name Pandoc
data Strand = Strand Name [Chapter]
data Novel = Novel Name [Strand]

class Document a where
	readDoc :: IOConfiged a
	flattenDoc :: a -> Configed [Block]
	writeDoc :: a -> IOConfiged ()
	writedoc x = do
		doc <- asks complete x
		options <- asks writerOptions
		latexFile <- asks destination Latex
		writeFile latexFile $ writeLatex options doc
		epubFile <- asks destination Epub x
		writeFile epubFile $ writeEpub options doc

instance Document Novel where
	readDoc :: IOConfiged Novel
	readDoc = do
		name <- asks name
		let loadStrand = readDoc . local strandConfig
		let readAll = lift $ mapM loadStrand
		strands <- readAll $ asks strandsOf
		return $ Novel name strands

	flattenDoc :: Novel -> Configed [Block]
	flattenDoc (Novel name strands) = do
		return $ concatmap $ flattenDoc strands

instance Document Strand where
	readDoc :: IOConfiged Strand
	readDoc = do
		name <- asks name
		let loadChapter = readDoc . local chapterConfig
		let readAll = lift $ mapM loadChapter
		chapters <- readAll $ asks chaptersOf
		return $ Strand name chapters
		
	flattenDoc :: Strand -> Configed [Block]
	flattenDoc (Strand name chapters) = do
		return $ concatmap $ flattenDoc chapters

instance Document Chapter where
	readDoc :: IOConfiged Chapter
	readDoc = do
		name <- asks name
		file <- asks file
		options <- asks readerOptions
		proc <- asks processor
		let doc = readMarkdown options file
		return $ Chapter name $ proc doc

	flattenDoc :: Chapter -> Configed [Block]
	flattenDoc (Chapter name (Pandoc meta blocks)) = return blocks

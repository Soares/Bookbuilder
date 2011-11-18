import Data.String.Utils (join)
import System.Directory (doesFileExist)
import System.FilePath.Posix (combine)
import System.Process (system)
import Control.Monad (filterM)


{- Type declarations -}
type Strand = String


{- Environment settings -}
strandDir = "strands"
strandFile = ".strands"
sectionFile = ".sections"
divider = "\n*\t*\t*\n"
compileDir = ".build"
outputDir = "pdf"
ext = ".md"
format = ".pdf"


{- Environment functions -}

-- The destination for the compiled markdown file
tmpFile :: Strand -> FilePath
tmpFile = combine compileDir . (++ ext)

-- The destination for the compiled pdf file
outFile :: Integer -> Strand -> FilePath
outFile n strand = combine outputDir (name ++ format) where
    name = twoDigit n ++ "-" ++ strand

-- Convert markdown to pdf
convert :: Integer -> Strand -> IO ()
convert n strand = do
    let input = tmpFile strand
    let output = outFile n strand
    system $ "markdown2pdf " ++ input ++ " -o " ++ output
    return ()


{- Agnostic utilities -}

-- Read a file into a list of lines
readLines :: FilePath -> IO [String]
readLines = fmap lines . readFile

-- The lines that are not comments
realLines :: [String] -> [String]
realLines = filter ((/=) '#' . head)

-- Combine all directory path fragments into a single directory path in a POSIX
-- compliant fashion
combineAll :: [FilePath] -> FilePath
combineAll (f:fs) = foldl combine f fs

-- Show an integer with at least two digits
twoDigit :: Integer -> String
twoDigit n | n < 10 && n >= 0 = '0' : show n
           | otherwise        = show n


{- Section manipulation -}

-- Get the names of all sections in a strand directory
sectionNames :: FilePath -> IO [String]
sectionNames dir = do
    lines <- readLines $ combine dir sectionFile
    let fix = combine dir . (++ ext)
    return $ fmap fix $ realLines lines

-- Get the full text of a strand, which is all sections combined
fullText :: Strand -> IO String
fullText strand = do
    let dir = combine strandDir strand
    sections <- sectionNames dir
    texts <- mapM readFile sections
    return $ join divider texts


{- Strand manipulation -}

-- Write the contents of a strand into a compiled markdown file
compile :: Strand -> IO ()
compile strand = fullText strand >>= writeFile (tmpFile strand)

-- Get all strands, in order
strandNames :: IO [Strand]
strandNames = do
    names <- readLines (combine strandDir strandFile)
    let sections strand = combineAll [strandDir, strand, sectionFile]
    filterM (doesFileExist . sections) $ realLines names


{- Main action -- convert each strand into a numbered pdf file -}
main :: IO ()
main = do
    strands <- strandNames
    mapM_ compile strands
    mapM_ (uncurry convert) $ zip [0..] strands
    return ()

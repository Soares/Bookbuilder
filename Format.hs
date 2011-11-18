module Format where
import System.FilePath.Posix (addExtension)
import Text.Pandoc ( Pandoc
                   , WriterOptions
                   , writeLaTeX
                   , writeEPUB )
import qualified Data.ByteString.Lazy as Byte

type OutputConfig = (FilePath, WriterOptions, Maybe String)

data Format = Format { writer :: OutputConfig -> Pandoc -> IO ()
                     , ext :: String }
instance Eq Format where
  x == y = ext x == ext y

path :: FilePath -> Format -> FilePath
path base format = addExtension (ext format) base

outputer :: OutputConfig -> Format -> Pandoc -> IO ()
outputer (base, options, style) format = writer format options' where
	options' = (path base format, options, style)

type SimpleWriter = WriterOptions -> Pandoc -> String

outputterFor :: SimpleWriter -> OutputConfig -> Pandoc -> IO ()
outputterFor write (path, options, _) = writeFile path . write options

-- Formats
toLaTeX = Format{ writer = outputterFor writeLaTeX, ext = "tex" }

toEPUB = Format { writer = doEPUB, ext = "epub" }
doEPUB (path, options, style) doc = do
	bytes <- writeEPUB style options doc
	Byte.writeFile path bytes

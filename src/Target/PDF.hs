module Target.PDF ( outputLaTeX ) where
-- Copied from Pandoc's markdown2pdf
-- TODO: the style here doesn't match Bookbuilder style
-- TODO: Add real error handling

import Prelude hiding ( catch )
import Data.List (isInfixOf, intercalate)
import Data.Configger ( Config )
import Data.Maybe ( isNothing, fromMaybe )
import Codec.Binary.UTF8.String (encodeString)
import qualified Data.ByteString as BS
import Data.ByteString.UTF8 (toString)
import Control.Monad (unless, guard, liftM)
import Control.Concurrent (putMVar, takeMVar, newEmptyMVar, forkIO)
import Control.Exception (catch, tryJust, bracket, evaluate)
import Control.Monad.Trans (liftIO)
import System.IO
import System.IO.Error (isDoesNotExistError, isAlreadyExistsError)
import System.Environment ( getProgName )
import System.Exit (ExitCode (..))
import System.FilePath
import System.Directory
import System.Process
import Target.Config ( latex )

-- A variant of 'readProcessWithExitCode' that does not
-- cause an error if the output is not UTF-8. (Copied
-- with slight variants from 'System.Process'.)
readProcessWithExitCode'
    :: FilePath                 -- ^ command to run
    -> [String]                 -- ^ any arguments
    -> String                   -- ^ standard input
    -> IO (ExitCode,String,String) -- ^ exitcode, stdout, stderr
readProcessWithExitCode' cmd args input = do
    (Just inh, Just outh, Just errh, pid) <-
        createProcess (proc cmd args){ std_in  = CreatePipe,
                                       std_out = CreatePipe,
                                       std_err = CreatePipe }

    outMVar <- newEmptyMVar

    -- fork off a thread to start consuming stdout
    out  <- liftM toString $ BS.hGetContents outh
    _ <- forkIO $ evaluate (length out) >> putMVar outMVar ()

    -- fork off a thread to start consuming stderr
    err  <- liftM toString $ BS.hGetContents errh
    _ <- forkIO $ evaluate (length err) >> putMVar outMVar ()

    -- now write and flush any input
    unless (null input) $ do hPutStr inh input; hFlush inh
    hClose inh -- done with stdin

    -- wait on the output
    takeMVar outMVar
    takeMVar outMVar
    hClose outh

    -- wait on the process
    ex <- waitForProcess pid

    return (ex, out, err)

run :: FilePath -> [String] -> IO (Either String String)
run file opts = do
  (code, out, err) <- readProcessWithExitCode' (encodeString file)
                        (map encodeString opts) ""
  let msg = out ++ err
  case code of
    ExitFailure _ -> return $ Left  $! msg
    ExitSuccess   -> return $ Right $! msg

runLatexRaw :: String -> FilePath -> IO (Either (Either String String) FilePath)
runLatexRaw latexProgram file = do
  -- we ignore the ExitCode because pdflatex always fails the first time
  run latexProgram ["-halt-on-error", "-interaction", "nonstopmode",
    "-output-directory", takeDirectory file, dropExtension file] >> return ()
  let pdfFile = replaceExtension file "pdf"
  let logFile = replaceExtension file "log"
  txt <- tryJust (guard . isDoesNotExistError)
         (liftM toString $ BS.readFile logFile)
  let  checks = checkLatex $ either (const "") id txt
  case checks of
  -- err  , bib , ref , msg
    (True , _    , _   , msg) -> return $ Left $ Left msg   -- failure
    (False, True , _   , msg) -> runBibtex file >>
                                 return ( Left $ Right msg) -- citations
    (False, _    , True, msg) -> return $ Left $ Right msg  -- references
    (False, False, False, _ ) -> return $ Right pdfFile     -- success

runLatex :: String -> FilePath -> IO (Either String FilePath)
runLatex latexProgram file = step 3
  where
  step n = do
    result <- runLatexRaw latexProgram file
    case result of
      Left (Left err) -> return $ Left err
      Left (Right _) | n > 1  -> step (n-1 :: Int)
      Right _ | n > 2 -> step (n-1 :: Int)
      Left (Right msg) -> return $ Left msg
      Right pdfFile   -> return $ Right pdfFile

checkLatex :: String -> (Bool, Bool, Bool, String)
checkLatex ""  = (True, False, False, "Could not read log file")
checkLatex txt = (err , bib, ref, unlines $! msgs ++ tips)
  where
  xs `oneOf` x = any (`isInfixOf` x) xs
  msgs = dropWhile (not . errorline) $ lines txt
  errorline ('!':_) = True
  errorline _ = False
  tips = checkPackages msgs
  err = any (oneOf ["!", "LaTeX Error:", "Latex Error:"]) msgs
  bib = any (oneOf ["Warning: Citation"
                   ,"Warning: There were undefined citations"]) msgs
  ref = any (oneOf ["Warning: Reference"
                   ,"Warning: Label"
                   ,"Warning: There were undefined references"
                   ]) msgs

checkPackages :: [String] -> [String]
checkPackages = concatMap chks
  where -- for each message, search 'pks' for matches and give a hint
  chks x = concatMap (chk x) pks
  chk x (k,v) = if sub k `isInfixOf` x then tip k v else []
  sub k   = "`" ++ k ++ ".sty' not found"
  tip k v = ["Please install the '" ++ k ++
             "' package from CTAN:", "  " ++ v]
  pks = [("ucs"
         ,"http://www.ctan.org/tex-archive/macros/latex/contrib/unicode/")
        ,("ulem"
         ,"http://www.ctan.org/tex-archive/macros/latex/contrib/misc/")
        ,("graphicx"
         ,"http://www.ctan.org/tex-archive/macros/latex/required/graphics/")
        ,("fancyhdr"
         ,"http://www.ctan.org/tex-archive/macros/latex/contrib/fancyhdr/")
        ,("array"
         ,"http://www.ctan.org/tex-archive/macros/latex/required/tools/")]

runBibtex :: FilePath -> IO (Either String FilePath)
runBibtex file = do
  let auxFile = replaceExtension file "aux"
  result <- run "bibtex" [auxFile]
  return $ either Left (const $ Right auxFile) result

warn :: String -> IO ()
warn x = do
  progName <- getProgName
  hPutStrLn stderr $ progName ++ ": " ++ x

saveOutput :: FilePath -> FilePath -> IO ()
saveOutput input output = copyFile (encodeString input) (encodeString output)

-- | Perform a function in a temporary directory and clean up.
withTempDir :: FilePath -> (FilePath -> IO a) -> IO a
withTempDir baseName = bracket (createTempDir 0 baseName) removeDirectoryRecursive

-- | Create a temporary directory with a unique name.
createTempDir :: Integer -> FilePath -> IO FilePath
createTempDir num baseName = do
  sysTempDir <- getTemporaryDirectory
  let dirName = sysTempDir </> baseName <.> show num
  liftIO $ catch (createDirectory dirName >> return dirName) $
      \e -> if isAlreadyExistsError e
               then createTempDir (num + 1) baseName
               else ioError e

outputLaTeX :: Config -> FilePath -> String -> IO ()
outputLaTeX conf path tex = withTempDir "bookbuilder" $ \tmp -> do
    let latexProgram = fromMaybe "pdflatex" (latex conf)
    let execs = [latexProgram, "bibtex"]
    paths <- mapM findExecutable execs
    let miss = map snd $ filter (isNothing . fst) $ zip paths execs
    unless (null miss) $ warn $! "Could not find " ++ intercalate ", " miss

    let texFile = replaceDirectory path tmp <.> "tex"
    writeFile texFile tex

    latexRes <- runLatex latexProgram texFile
    case latexRes of
        Left err      -> warn err
        Right pdfFile -> saveOutput pdfFile path

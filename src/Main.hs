module Main where
import Control.Dangerous ( runDangerous, Exit(..), Warning(..) )
import System.Environment
import System.Exit
import System.IO
import Text.Printf
-- Bookbuilder packages
import Options

warn :: Warning -> IO ()
warn (Warning s) = hPutStrLn stderr $ "Warning: " ++ show s

exit :: Exit -> IO ()
exit (Stop s) = do
    name <- getProgName
    putStrLn $ printf "%s: %s" name (show s)
    exitWith ExitSuccess
exit (Failure e) = do
    hPutStrLn stderr $ "Error: " ++ show e
    exitWith $ ExitFailure 1

main :: IO ()
main = do
    name <- getProgName
    argv <- getArgs
    let (result, warnings) = runDangerous $ configure name argv
    mapM_ warn warnings
    case result of
        Left stop -> exit stop
        Right config -> print config

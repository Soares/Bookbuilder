module Main where
import Control.Dangerous
import System.Environment
import System.Exit
import System.IO
import Text.Printf
-- Bookbuilder packages
import Options ( configure )
import Book ( load )

main :: IO ()
main = do
    name <- getProgName
    argv <- getArgs
    book <- execute $ runDangerous $ load =<< configure name argv
    compile book

module Main where
import Control.Dangerous
import System.Environment
import Options ( configure )
import Book ( load, compile )

main :: IO ()
main = do
    name <- getProgName
    argv <- getArgs
    opts <- execute $ runDangerous $ configure name argv
    book <- execute =<< runDangerousT (load opts)
    compile book

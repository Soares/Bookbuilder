import Directory (getCurrentDirectory)
import Structure (discover, output)

main :: IO ()
main = do
	here <- getCurrentDirectory
	structure <- discover here
	output structure

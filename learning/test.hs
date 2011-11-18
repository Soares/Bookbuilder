import Control.Monad.Reader
data Document = Novel (String, String)
              | Strand (String, Integer)
							| Chapter String
data Config = Config { someA :: String
                     , someB :: String }
							
type Packaged = Reader Config

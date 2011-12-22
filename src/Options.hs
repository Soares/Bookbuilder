module Options ( Options, configure {-, environment-} ) where

import Control.Monad ( when, unless )
import Control.Monad.Loops ( concatM )
import Control.Monad.Error ( ErrorT, Error(..), throwError )
import Control.Monad.State ( State )
import Control.Monad.Reader ( ReaderT(runReaderT) )
import Dangerous ( warn )
import Data.Focus ( Focus, parse, unfocused )
import System.Console.GetOpt
import System.Exit ( exitWith, ExitCode (..) )
import System.Environment ( getArgs, getProgName )
import System.IO ( hPutStrLn, stderr )
-- import Text.Bookbuilder ( targets, output )
-- import Text.Bookbuilder.Config ( Options(..), configure, Config )
-- import Text.Bookbuilder.Location ( Location, nowhere )


data Options = Options
    { optRoot      :: FilePath
    , optSourceDir :: FilePath
    , optTargetDir :: FilePath
    , optBuildDir  :: FilePath
    , optStart     :: Maybe Focus
    , optEnd       :: Maybe Focus
    , optTargets   :: [String]
    , optIsWithin  :: Bool
    , optDetect    :: Bool
    , optDebug     :: Bool
    , optHelp      :: Bool
    } deriving Show


defaultOptions :: Options
defaultOptions = Options
    { optRoot      = ""
    , optSourceDir = "src"
    , optTargetDir = "targets"
    , optBuildDir  = "build"
    , optStart     = Nothing
    , optEnd       = Nothing
    , optTargets   = []
    , optIsWithin  = False
    , optDetect    = True
    , optDebug     = False
    , optHelp      = False }


-- | A list of functions, each transforming the options data structure
-- | in response to a command-line option.
options :: [OptDescr (Options -> Dangerous Options)]
options =
    [ Option [] ["src"]
        (ReqArg (\arg opt -> return opt{ optSourceDir = arg }) "DIR")
        "directory containing the book source files"

    , Option [] ["targets"]
        (ReqArg (\arg opt -> return opt{ optTargetDir = arg }) "DIR")
        "directory in which to place compiled files"

    , Option [] ["build"]
        (ReqArg (\arg opt -> return opt{ optBuildDir = arg }) "DIR")
        "directory in which to place compiled files"

    , Option "t" ["target"]
        (ReqArg (\arg opt -> return
			opt{ optTargets = arg : optTargets opt }) "PROF")
        "the target to be built (may be specified multiple times)"

    , Option "b" ["begin"]
        (OptArg (\arg opt -> maybe (return unfocused) parseLOC arg >>=
			\foc -> return opt{ optStart = Just foc }) "LOC")
        "the section to start at, i.e. 01-02 or 2.3.1"

    , Option "e" ["end"]
        (OptArg (\arg opt -> maybe (return unfocused) parseLOC arg >>=
			\foc -> return opt{ optEnd = Just foc }) "LOC")
        "the section to build up to, i.e. 4 or 02_04"

    , Option "n" ["only"]
        (OptArg (\arg opt -> maybe (return unfocused) parseLOC arg >>=
			\foc -> return opt{ optStart = Just foc, optEnd = Just foc }) "LOC")
        "the specific section to compile"

    , Option [] ["nodetect"]
        (NoArg (\opt -> return opt{ optDetect = True }))
        "don't try to detect the book from the current location"

    , Option "d" ["debug"]
        (NoArg (\opt -> return opt{ optDebug = True }))
        "write the intermediary files, i.e. default.pdf.tex"

    , Option "h?" ["help"]
        (NoArg (\opt -> return opt { optHelp = True}))
        "display this help"
    ]


-- | Parse a LOC into a Focus, i.e. 03-01-01 -> <3|3|1>
parseLOC :: String -> Dangerous Focus
parseLOC arg = case parse arg of
    ((f, ""):_) -> return f
    _ -> throwError $ ParseError msg
    where msg = "Please enter locations in the form of underscore-separated numbers, i.e. 3_1_5"


configure :: [String] -> Dangerous Options
configure argv = do
    let (actions, args, optErrors) = getOpt Permute options argv
    unless (null optErrors) (throwError $ FormatErrors optErrors)
    when (length args > 1) (warn $ ExtraArguments $ tail args)
    let options = if null args
        then defaultOptions
        else defaultOptions{ optRoot = head args }
    concatM actions options

type Dangerous = ErrorT OptionError (State [OptionWarning])
data OptionError = FormatErrors [String] | ParseError String deriving Show
data OptionWarning = ExtraArguments [String] deriving Show
instance Error OptionError where strMsg = ParseError

#!/usr/bin/env cabal

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

{- cabal:
ghc-options:
  -O2 -threaded -rtsopts "-with-rtsopts=-N -T"
  -Wall -Wcompat -Wincomplete-record-updates
  -Wincomplete-uni-patterns -Wredundant-constraints
build-depends:
    base
  , ansi-terminal        ^>= 0.11
  , bytestring           ^>= 0.10.10.0
  -- , cabal-fmt            ^>= 0.1.5.1
  , optparse-applicative ^>= 0.16.0.0
  , ormolu               ^>= 0.1.3.1
  , path                 ^>= 0.8.0
  , path-io              ^>= 1.6.2
  , process              ^>= 1.6.10.0
  , safe-exceptions      ^>= 0.1.7.1
  , streamly             ^>= 0.7.2
  , temporary            ^>= 1.3
  , text                 ^>= 1.2.3.2
-}

{-
FORMATTING UTILITY
This script runs both Ormolu and "cabal-fmt" as formatting stages that can
be used to both:
  - format files in the directory tree of a project, and
  - check that files are formatted for CI purposes

NOTE: cabal-fmt must be installed separately

To format files:
```
cd <project-directory>
./format.hs format  # you can also omit the command format
```
To check files in CI:
```
cd <project-directory>
./format.hs check
```
Feel free to extend this script to become a more polished utility.
LICENSE: BSD-3-Clause
ORIGINAL AUTHOR: Jonathan Merritt (j.s.merritt@gmail.com)
-}

import Control.Exception.Safe (Exception, catch, displayException)
import qualified Data.ByteString as ByteString
import Data.Function ((&))
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Text.Encoding (decodeUtf8, encodeUtf8)
import qualified Data.Text.IO as Text.IO
import GHC.Conc (getNumCapabilities)
import qualified Options.Applicative as OA
import qualified Ormolu
import Path (Abs, Dir, File, Path)
import qualified Path
import qualified Path.IO
import Streamly (AsyncT, asyncly, maxThreads, serially)
import qualified Streamly.Prelude as S
import qualified System.Console.ANSI as ANSI
import System.Exit (ExitCode (ExitFailure, ExitSuccess), exitFailure, exitSuccess)
import System.IO (hClose)
import System.IO.Unsafe (unsafePerformIO)
import qualified System.Process as Process

main :: IO ()
main = do
  let fileFilter :: FileFilter = excludeHidden <> excludeDistNewstyle
      formatter :: Formatter = ormoluFormatter <> cabalFormatter

  let opts =
        OA.info
          (cliParser OA.<**> OA.helper)
          ( OA.fullDesc
              <> OA.progDesc "Formatting Utility"
              <> OA.header "format - formatting stuff for Haskell projects"
          )
  CLIArgs runMode <- OA.execParser opts
  let txtMode =
        case runMode of
          CheckOnly -> "checking only"
          FormatFiles -> "file formatting"

  numCap <- getNumCapabilities

  let threads = numCap + 2
      formatWidth = 65

  Text.IO.putStrLn "== FORMATTER UTILITY =="
  Text.IO.putStrLn $ "Running in " <> txtMode <> " mode"
  Text.IO.putStrLn $ "Setting threadpool size to: " <> (Text.pack . show) threads
  dir <- Path.IO.getCurrentDir
  Text.IO.putStrLn $ "Current directory: " <> (Text.pack . Path.fromAbsDir) dir

  -- streamly action
  --
  -- What's going on here:
  --   1. A stream provides a list of files recursively (listDirRecursive)
  --   2. The stream is filtered so that only acceptable file paths (eg. not
  --      hidden directories or dist-newstyle files, etc) are included.
  --   3. These files are processed in parallel (in basically a threadpool) so
  --      that formatting can be applied to them (runFormatIO). Whether or not
  --      the formatting is actually applied or just checked is determined by
  --      the runMode.
  --   4. The results of formatting are collected to be logged by a single
  --      thread.
  --
  -- We return a check to see if any files changed (or would have changed in
  -- checking mode).
  changed :: [Bool] <-
    -- flag if any files change
    S.toList $
      S.map (isUnchanged . snd) $
        -- this "serially" collects the results of the inner async operation into
        -- a single thread for tracing
        serially
          ( asyncly . maxThreads threads $
              ( listDirRecursive dir
                  & S.filter (acceptPath fileFilter)
                  & S.mapM (runFormatIO runMode formatter)
              )
          )
          & S.trace (logLine UseANSI formatWidth dir)
  let noChange = and changed

  case runMode of
    FormatFiles -> exitSuccess
    CheckOnly -> do
      if noChange
        then do
          Text.IO.putStrLn "All files passed formatting checks!"
          exitSuccess
        else do
          Text.IO.putStrLn "File(s) failed formatting checks!"
          exitFailure

---- Ormolu Formatter

ormoluFormatter :: Formatter
ormoluFormatter = Formatter $ \filePath ->
  if extensionIs ".hs" filePath
    then Format (ormoluFormatAction Ormolu.defaultConfig filePath)
    else NoFormat

ormoluFormatAction ::
  Ormolu.Config Ormolu.RegionIndices ->
  Path Abs File ->
  Text ->
  FormattingResult
ormoluFormatAction config filePath contents =
  unsafePerformIO $ do
    let stringContents = Text.unpack contents
        fileName = Path.fromAbsFile filePath
    eitherOutcome <-
      catch
        (Right <$> Ormolu.ormolu config fileName stringContents)
        ( \(e :: Ormolu.OrmoluException) ->
            pure . Left $ exceptionToFormattingResult e
        )
    case eitherOutcome of
      Left err -> pure err
      Right outputText ->
        pure $
          if outputText == contents
            then FormattingUnchanged
            else FormattingChanged outputText

---- Cabal Formatter

cabalFormatter :: Formatter
cabalFormatter = Formatter $ \filePath ->
  if extensionIs ".cabal" filePath
    then Format cabalFormatAction
    else NoFormat

cabalFormatAction :: Text -> FormattingResult
cabalFormatAction contents =
  unsafePerformIO $
    Path.IO.withSystemTempFile ".cabal" $
      \filePath handle -> do
        hClose handle
        writeFileUtf8 filePath contents
        let cp =
              Process.proc
                "cabal-fmt"
                ["--inplace", Path.fromAbsFile filePath]
        (exitCode, _, stderr) <- Process.readCreateProcessWithExitCode cp ""
        case exitCode of
          ExitFailure _ -> pure . FormattingError . Text.pack $ stderr
          ExitSuccess -> do
            outputText <- readFileUtf8 filePath
            pure $
              if outputText == contents
                then FormattingUnchanged
                else FormattingChanged outputText

---- CLI parser

newtype CLIArgs = CLIArgs {_cliArgsRunMode :: RunMode}

cliParser :: OA.Parser CLIArgs
cliParser =
  CLIArgs
    <$> OA.subparser
      ( OA.command "check" (OA.info (pure CheckOnly) (OA.progDesc "Only check file formatting"))
          <> OA.command "format" (OA.info (pure FormatFiles) (OA.progDesc "Format files"))
      )
      OA.<|> (pure . CLIArgs $ FormatFiles)

---- Text logging

data ANSI = UseANSI | NoANSI

data Color = Green | Yellow | Red

logLine :: ANSI -> Int -> Path Abs Dir -> (Path Abs File, FormattingResult) -> IO ()
logLine ansi colWidth dir (filePath, result) =
  if result == NotFormatted
    then pure ()
    else do
      relFile <- Path.stripProperPrefix dir filePath
      let fileTxt = Text.pack $ Path.fromRelFile relFile
          formattedFile = Text.justifyLeft colWidth '.' fileTxt
      Text.IO.putStr formattedFile
      case result of
        FormattingUnchanged -> inSquareBrackets ansi Green "unchanged"
        FormattingChanged _ -> inSquareBrackets ansi Yellow "re-formatted"
        FormattingError errorMessage -> do
          inSquareBrackets ansi Red "error"
          Text.IO.putStr "\n"
          Text.IO.putStrLn errorMessage
        NotFormatted -> error "should not occur"
      Text.IO.putStr "\n"

inSquareBrackets :: ANSI -> Color -> Text -> IO ()
inSquareBrackets ansi color text = do
  Text.IO.putStr "["
  putInColor ansi color text
  Text.IO.putStr "]"

putInColor :: ANSI -> Color -> Text -> IO ()
putInColor ansi color text =
  case ansi of
    NoANSI -> Text.IO.putStr text
    UseANSI ->
      let ansiColor =
            case color of
              Green -> ANSI.Green
              Yellow -> ANSI.Yellow
              Red -> ANSI.Red
       in do
            ANSI.setSGR [ANSI.SetColor ANSI.Foreground ANSI.Vivid ansiColor]
            Text.IO.putStr text
            ANSI.setSGR [ANSI.Reset]

---- Formatting in IO

-- | Run a formatter in IO on a single file path.
runFormatIO ::
  RunMode ->
  Formatter ->
  Path Abs File ->
  IO (Path Abs File, FormattingResult)
runFormatIO runMode formatter filePath =
  case canFormat formatter filePath of
    NoFormat -> pure (filePath, NotFormatted)
    Format f -> do
      content <- readFileUtf8 filePath
      result <-
        case f content of
          NotFormatted -> pure NotFormatted
          FormattingUnchanged -> pure FormattingUnchanged
          changed@(FormattingChanged content') -> do
            case runMode of
              CheckOnly -> pure ()
              FormatFiles -> writeFileUtf8 filePath content'
            pure changed
          err@(FormattingError _errorMessage) -> pure err
      pure (filePath, result)

---- Running mode

data RunMode = CheckOnly | FormatFiles

---- Formatters and associated types

-- | Result of running a formatter.
data FormattingResult
  = NotFormatted
  | FormattingUnchanged
  | FormattingChanged !Text
  | FormattingError !Text
  deriving (Eq)

-- | Indicate whether formatting a file should proceed or not.
data FormattingDirective
  = NoFormat
  | Format (Text -> FormattingResult)

-- | Formatter.
newtype Formatter = Formatter {canFormat :: Path Abs File -> FormattingDirective}

-- | Check if a formatting result is unchanged.
isUnchanged :: FormattingResult -> Bool
isUnchanged fr =
  case fr of
    NotFormatted -> True
    FormattingUnchanged -> True
    FormattingChanged _ -> False
    FormattingError _ -> False

-- | Convert any Exception into a FormattingResult using displayException.
exceptionToFormattingResult :: Exception e => e -> FormattingResult
exceptionToFormattingResult = FormattingError . Text.pack . displayException

-- | Sequentially compose a pair of formatting functions.
sequenceFormatting ::
  (Text -> FormattingResult) ->
  (Text -> FormattingResult) ->
  (Text -> FormattingResult)
sequenceFormatting f1 f2 = \content ->
  case f1 content of
    NotFormatted -> f2 content
    FormattingUnchanged -> f2 content
    FormattingChanged content' -> f2 content'
    err@(FormattingError _errorMessage) -> err

-- | The semigroup combination for Formatter runs the formatters in sequence,
--   if they accept the given file path.
instance Semigroup Formatter where
  fmt1 <> fmt2 = Formatter $ \filePath ->
    case canFormat fmt1 filePath of
      NoFormat -> canFormat fmt2 filePath
      Format f1 ->
        case canFormat fmt2 filePath of
          NoFormat -> Format f1
          Format f2 -> Format $ sequenceFormatting f1 f2

-- | The empty Formatter does not format anything.
instance Monoid Formatter where
  mempty = Formatter $ const NoFormat

---- Directory, file and path utilities

newtype FileFilter = FileFilter {unFileFilter :: Path Abs File -> [Text] -> Bool}

instance Semigroup FileFilter where
  ff1 <> ff2 = FileFilter $ \filePath components ->
    unFileFilter ff1 filePath components
      && unFileFilter ff2 filePath components

instance Monoid FileFilter where
  mempty = FileFilter $ \_ _ -> True

excludeHidden :: FileFilter
excludeHidden = FileFilter $ \_ components ->
  not $ any (Text.isPrefixOf ".") components

excludeDistNewstyle :: FileFilter
excludeDistNewstyle = FileFilter $ \_ components ->
  not $ "dist-newstyle" `elem` components

acceptPath :: FileFilter -> Path Abs File -> Bool
acceptPath ff filePath = unFileFilter ff filePath components
  where
    components = pathAbsFileComponents filePath

listDirRecursive :: Path Abs Dir -> AsyncT IO (Path Abs File)
listDirRecursive dir = do
  (subdirs, files) <- Path.IO.listDir dir
  S.fromList files <> foldMap listDirRecursive subdirs

extensionIs :: Text -> Path Abs File -> Bool
extensionIs e filePath = ext == e
  where
    ext =
      case Path.fileExtension filePath of
        Nothing -> ""
        Just exte -> Text.pack exte

readFileUtf8 :: Path Abs File -> IO Text
readFileUtf8 filePath =
  decodeUtf8 <$> ByteString.readFile (Path.fromAbsFile filePath)

writeFileUtf8 :: Path Abs File -> Text -> IO ()
writeFileUtf8 filePath content =
  ByteString.writeFile (Path.fromAbsFile filePath) (encodeUtf8 content)

pathAbsFileComponents :: Path Abs File -> [Text]
pathAbsFileComponents filePath =
  reverse $ relFile (Path.filename filePath) : go (Path.parent filePath)
  where
    go d
      | isRoot d = []
      | otherwise = relDir (Path.dirname d) : go (Path.parent d)
    relFile = Text.pack . Path.fromRelFile
    relDir = Text.init . Text.pack . Path.fromRelDir

isRoot :: Path Abs Dir -> Bool
isRoot dir = Path.fromAbsDir dir == "/"

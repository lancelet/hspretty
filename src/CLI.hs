{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- |
-- Description : Command-line interface.
--
-- This module performs the command-line interface for the formatter, and
-- plumbs formatting actions through a streaming interface.
module CLI where

import Data.Function ((&))
import qualified Data.Text.Short as ShortText
import Formatter (Formatter, runFormatIO)
import qualified Formatter
import qualified Formatters.CabalFmt (formatter)
import qualified Formatters.Ormolu (formatter)
import qualified GHC.Conc
import qualified Log
import Options.Applicative (Parser)
import qualified Options.Applicative as OA
import Path (Abs, Dir, File, Path)
import qualified Path
import qualified Path.IO
import PathFilter (PathFilter)
import qualified PathFilter
import RunMode (RunMode)
import qualified RunMode
import Streamly (AsyncT, asyncly, maxThreads, serially)
import qualified Streamly.Prelude as S
import qualified System.Exit

-- | Main entry point.
run :: IO ()
run = do
  -- parse command-line arguments
  args <- parseArgs

  -- set the default logger
  let lg = Log.defaultLog

  -- fetch number of capabilities; to decide the number of threads
  numCap <- GHC.Conc.getNumCapabilities
  let nThreads = numCap + 2

  -- fetch the current directory to use as the parent directory
  dir <- Path.IO.getCurrentDir

  -- tell the user what we're doing
  let txtMode =
        case runMode args of
          RunMode.Format -> "file formatting"
          RunMode.CheckOnly -> "checking only"
  Log.info lg "# Formatter Utility"
  Log.info lg $ "Mode             : " <> txtMode
  Log.info lg $
    "Threadpool size  : "
      <> (ShortText.pack . show $ nThreads)
  Log.info lg $
    "Parent directory : "
      <> (ShortText.pack . Path.fromAbsDir $ dir)

  -- streamly action
  changed :: [Bool] <-
    S.toList $
      S.map (Formatter.isUnchanged . snd) $
        serially
          ( asyncly . maxThreads nThreads $
              ( listDirRecursive dir
                  & S.map (fromJustUnsafe . Path.stripProperPrefix dir)
                  & S.filter
                    ( PathFilter.toBool
                        . PathFilter.unPathFilter pathFilter
                    )
                  & S.mapM
                    ( \relFile -> do
                        result <-
                          Formatter.runFormatIO
                            (runMode args)
                            formatter
                            dir
                            relFile
                        pure (relFile, result)
                    )
              )
          )
          & S.trace (uncurry (Log.report lg))
  let noChange = and changed

  -- report on the overall outcome if necessary
  case runMode args of
    RunMode.Format -> System.Exit.exitSuccess
    RunMode.CheckOnly -> do
      if noChange
        then do
          Log.info lg "Done: All files passed formatting checks."
          System.Exit.exitSuccess
        else do
          Log.info lg "Done: File(s) failed formatting checks."
          System.Exit.exitFailure

-- | Default path filter.
pathFilter :: PathFilter
pathFilter =
  PathFilter.pfNoHidden
    <> PathFilter.pfNoDistNewstyle

-- | Default formatter.
formatter :: Formatter
formatter =
  Formatters.Ormolu.formatter
    <> Formatters.CabalFmt.formatter

-- | fromJust.
fromJustUnsafe :: Maybe a -> a
fromJustUnsafe (Just x) = x
fromJustUnsafe Nothing = error "fromJustUnsafe: not Just!"

-- | List a directory recursively for streaming.
listDirRecursive :: Path Abs Dir -> AsyncT IO (Path Abs File)
listDirRecursive dir = do
  (subdirs, files) <- Path.IO.listDir dir
  S.fromList files <> foldMap listDirRecursive subdirs

-- | Command-line arguments.
newtype Args = Args
  { -- Run mode.
    runMode :: RunMode
  }

-- | Parse command-line arguments.
parseArgs :: IO Args
parseArgs = OA.execParser opts
  where
    opts =
      OA.info
        (parser OA.<**> OA.helper)
        ( OA.fullDesc
            <> OA.progDesc "Formatting Utility"
            <> OA.header "format - formatting stuff for Haskell projects"
        )

-- | Parser for command-line arguments.
parser :: Parser Args
parser =
  Args
    <$> OA.subparser
      ( OA.command
          "check"
          ( OA.info
              (pure RunMode.CheckOnly)
              (OA.progDesc "Only check file formatting (do not over-write)")
          )
          <> OA.command
            "format"
            ( OA.info
                (pure RunMode.Format)
                (OA.progDesc "Format files (over-write if required)")
            )
      )

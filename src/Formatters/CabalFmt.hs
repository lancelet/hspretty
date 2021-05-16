{-# LANGUAGE OverloadedStrings #-}

-- |
-- Description : @cabal-fmt@ formatter.
--
-- Applies the @cabal-fmt@ formatter.
module Formatters.CabalFmt where

import qualified Data.Text.Short as ShortText
import Formatter (ErrorMessage, FileContent, Formatter, FormattingResult)
import qualified Formatter
import qualified Path
import qualified Path.IO
import PathFilter (PathFilter)
import qualified PathFilter
import qualified System.Directory
import qualified System.Exit
import qualified System.IO.Unsafe (unsafePerformIO)
import qualified System.Process
import qualified UnliftIO.IO

-- | cabal-fmt formatter.
formatter :: Formatter
formatter = Formatter.Formatter $ \path ->
  case PathFilter.unPathFilter pfCabal path of
    PathFilter.Reject -> Formatter.DoNotFormat
    PathFilter.Accept -> Formatter.Format formatAction

-- | Formatting action; defers to a 'System.IO.Unsafe.unsafePerformIO' action.
formatAction :: FileContent -> FormattingResult FileContent
formatAction content = System.IO.Unsafe.unsafePerformIO $ formatActionIO content

-- | Formatting action in 'IO'.
formatActionIO :: FileContent -> IO (FormattingResult FileContent)
formatActionIO content = do
  -- check that we have the cabal-fmt utility installed first
  cabalFmtCheck <- checkForCabalFmt
  case cabalFmtCheck of
    Left err -> pure . Formatter.Error $ err
    Right () -> do
      -- create a temp file for the cabal file that must be formatted and
      --  invoke cabal-fmt on that
      Path.IO.withSystemTempFile ".cabal" $
        \tempFile handle -> do
          UnliftIO.IO.hClose handle
          writeResult <- Formatter.writeAbsoluteFile tempFile content
          case writeResult of
            Left errorMessage -> pure . Formatter.Error $ errorMessage
            Right () -> do
              let cp =
                    System.Process.proc
                      "cabal-fmt"
                      ["--inplace", Path.fromAbsFile tempFile]
              (exitCode, _, stderr) <-
                System.Process.readCreateProcessWithExitCode cp ""
              case exitCode of
                System.Exit.ExitFailure _ ->
                  pure
                    . Formatter.Error
                    . Formatter.ErrorMessage
                    . ShortText.pack
                    $ stderr
                System.Exit.ExitSuccess -> do
                  readResult <- Formatter.readAbsoluteFile tempFile
                  case readResult of
                    Left errorMessage -> pure . Formatter.Error $ errorMessage
                    Right outContent ->
                      pure $
                        if outContent == content
                          then Formatter.Unchanged
                          else Formatter.Changed outContent

-- | Check for the @cabal-fmt@ executable, producing an error message if it is
--   not installed.
checkForCabalFmt :: IO (Either ErrorMessage ())
checkForCabalFmt = do
  exe <- System.Directory.findExecutable "cabal-fmt"
  case exe of
    Nothing ->
      pure . Left . Formatter.ErrorMessage $
        "Could not find executable \"cabal-fmt\"; please make sure it is "
          <> "available on the path."
    Just _ -> pure . Right $ ()

-- | A 'PathFilter' for @.cabal@ files.
pfCabal :: PathFilter
pfCabal = PathFilter.pfExtension ".cabal"

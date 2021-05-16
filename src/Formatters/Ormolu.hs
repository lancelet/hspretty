{-# LANGUAGE OverloadedStrings #-}

-- |
-- Description : Ormolu formatter.
--
-- Applies the Ormolu formatter.
module Formatters.Ormolu (formatter) where

import qualified Data.Text as Text
import qualified Data.Text.Short as ShortText
import Formatter (FileContent, Formatter, FormattingResult)
import qualified Formatter
import qualified Ormolu
import Path (File, Path, Rel)
import qualified Path
import PathFilter (PathFilter)
import qualified PathFilter
import qualified System.IO.Unsafe (unsafePerformIO)
import qualified UnliftIO

-- | Ormolu formatter.
formatter :: Formatter
formatter = Formatter.Formatter $ \path ->
  case PathFilter.unPathFilter pfHs path of
    PathFilter.Reject -> Formatter.DoNotFormat
    PathFilter.Accept ->
      Formatter.Format $ formatAction Ormolu.defaultConfig path

-- | Formatting action; defers to a 'System.IO.Unsafe.unsafePerformIO' action.
formatAction ::
  Ormolu.Config Ormolu.RegionIndices ->
  Path Rel File ->
  FileContent ->
  FormattingResult FileContent
formatAction config filePathForMessages content =
  System.IO.Unsafe.unsafePerformIO $
    formatActionIO config filePathForMessages content

-- | Formatting action in 'IO'.
formatActionIO ::
  Ormolu.Config Ormolu.RegionIndices ->
  Path Rel File ->
  FileContent ->
  IO (FormattingResult FileContent)
formatActionIO config filePathForMessages content = do
  case Formatter.fileContentToUtf8 content (Just filePathForMessages) of
    Left errorMessage -> pure $ Formatter.Error errorMessage
    Right txtContent -> do
      let strContent = Text.unpack txtContent
          strFileName = Path.fromRelFile filePathForMessages

          fmtAction :: IO FileContent
          fmtAction =
            Formatter.utf8TextToFileContent
              <$> Ormolu.ormolu config strFileName strContent

          recovery :: Ormolu.OrmoluException -> IO (FormattingResult FileContent)
          recovery e =
            pure . Formatter.Error . Formatter.ErrorMessage . ShortText.pack $
              "hspretty: Ormolu error when formatting file \""
                <> Path.fromRelFile filePathForMessages
                <> "\": "
                <> UnliftIO.displayException e

      result <- UnliftIO.catch (Right <$> fmtAction) (fmap Left <$> recovery)

      case result of
        Left err -> pure err
        Right outFileContent ->
          pure $
            if outFileContent == content
              then Formatter.Unchanged
              else Formatter.Changed outFileContent

-- | A 'PathFilter' for @.hs@ files.
pfHs :: PathFilter
pfHs = PathFilter.pfExtension ".hs"

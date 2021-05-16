{-# LANGUAGE OverloadedStrings #-}

-- |
-- Description : Formatter types and operations.
--
-- 'Formatter' is a (potential) formatter for a file. It contains a method,
-- 'runFormat', which takes a relative path to a file and, by inspecting the
-- path alone, must decide whether to format the file or not. To specify if the
-- file will be formatted by that formatter, it returns a 'FormattingDirective'.
--
-- If the 'FormattingDirective' is 'Format' then that constructor supplies a
-- function which can take the 'FileContent', and return a 'FormattingResult'.
-- In turn, the 'FormattingResult' specifies the result of formatting.
--
-- To implement a new 'Formatter', return a new 'Formatter' instance, which
-- inspects the file path and, if the file can be formatted by that 'Formatter',
-- return a 'Format' constructor containing the formatting operation.
--
-- All aspects of the 'Formatter' operation are pure or "effectively pure". If
-- 'IO' operations are required, they should be implemented in
-- 'System.IO.Unsafe.unsafePerformIO' as effectively-pure operations.
module Formatter
  ( -- * Types

    -- ** Formatting
    Formatter (..),
    FormattingDirective (..),
    FormattingResult (..),

    -- ** Miscellaneous
    FileContent (..),
    ErrorMessage (..),

    -- * IO Actions
    runFormatIO,

    -- * Functions

    -- ** Conversions
    fileContentToUtf8,
    utf8TextToFileContent,
  )
where

import Data.ByteString (ByteString)
import qualified Data.ByteString as ByteString
import Data.Text (Text)
import qualified Data.Text.Encoding as Encoding
import Data.Text.Short (ShortText)
import qualified Data.Text.Short as ShortText
import Path (Abs, Dir, File, Path, Rel, (</>))
import qualified Path
import RunMode (RunMode)
import qualified RunMode
import UnliftIO (IOException)
import qualified UnliftIO

-- | Formatter.
newtype Formatter = Formatter
  { -- | Run the formatter.
    --
    -- This accepts a relative path to a file and returns a formatting
    -- directive for that file. This is a pure function: it can only inspect
    -- the name of the file, it should NOT try to perform any IO.
    runFormat :: Path Rel File -> FormattingDirective
  }

-- | Formatting directive.
--
-- This indicates whether formatting can proceed or not.
data FormattingDirective
  = -- | Do not format the file any further.
    DoNotFormat
  | -- | Formatter, which, given the content of a file returns a formatting
    --   result.
    --
    -- This is a pure function. For some formatters, it may be necessary to run
    -- this action using 'System.IO.Unsafe.unsafePerformIO', but in that case,
    -- every effort should still be made to ensure it behaves as a pure
    -- function.
    Format (FileContent -> FormattingResult FileContent)

-- | Result of running a formatter.
--
-- The type parameter @a@ is the type of values returned when formatting has
-- changed.
data FormattingResult a
  = -- | The formatter decided not to format the file after inspecting it.
    NotFormatted
  | -- | Formatting completed successfully, without changes.
    Unchanged
  | -- | Formatting completed successfully, and there are new contents.
    Changed !a
  | -- | An error occurred while formatting.
    Error !ErrorMessage
  deriving (Eq)

-- | Content of a file for formatting.
newtype FileContent = FileContent
  { unFileContent :: ByteString
  }
  deriving (Eq)

-- | Error message.
newtype ErrorMessage = ErrorMessage
  { unErrorMessage :: ShortText
  }
  deriving (Eq)

-- | Run a formatter in IO on a single file.
--
-- This operation checks if the formatter can run on the provided file. If it
-- can run then the file is loaded, and the formatter is run. If the run mode is
-- 'RunMode.Format' then the new formatted output is written to the file,
-- otherwise the file is left as-is.
runFormatIO ::
  -- | Run mode: either we're only checking, or we're also formatting.
  RunMode ->
  -- | Formatter to run.
  Formatter ->
  -- | Parent / project directory.
  Path Abs Dir ->
  -- | Path to the file (relative to the above parent directory).
  Path Rel File ->
  -- | Formatting result (without capturing the formatted output).
  IO (FormattingResult ())
runFormatIO runMode formatter parentDir file =
  case runFormat formatter file of
    DoNotFormat -> pure NotFormatted
    Format formatFn -> do
      readResult <- readRelativeFile parentDir file
      case readResult of
        Left message -> pure (Error message)
        Right content ->
          case formatFn content of
            NotFormatted -> pure NotFormatted
            Unchanged -> pure Unchanged
            Error message -> pure (Error message)
            Changed newContent -> do
              case runMode of
                RunMode.CheckOnly -> pure (Changed ())
                RunMode.Format -> do
                  writeResult <- writeRelativeFile parentDir file newContent
                  case writeResult of
                    Left message -> pure (Error message)
                    Right () -> pure (Changed ())

-- | Read a relative file into 'FileContent'.
--
-- If the action is unsuccessful then an 'ErrorMessage' is returned.
readRelativeFile ::
  -- | Path to the parent directory of the file.
  Path Abs Dir ->
  -- | Path of the file relative to the parent directory.
  Path Rel File ->
  -- | IO action containing the file content.
  IO (Either ErrorMessage FileContent)
readRelativeFile parentDir file = UnliftIO.catchIO action recover
  where
    action :: IO (Either ErrorMessage FileContent)
    action = Right . FileContent <$> ByteString.readFile path

    recover :: IOException -> IO (Either ErrorMessage FileContent)
    recover ioe = pure . Left . ErrorMessage $ message
      where
        message :: ShortText
        message = "hspretty: Error reading file: " <> exceptionMessage

        exceptionMessage :: ShortText
        exceptionMessage = ShortText.pack . UnliftIO.displayException $ ioe

    path :: FilePath
    path = relativeFilePath parentDir file

-- | Write a relative file from 'FileContent'.
--
-- If the action is unsuccessful then an 'ErrorMessage' is returned.
writeRelativeFile ::
  -- | Path of the parent directory of the file.
  Path Abs Dir ->
  -- | Path of the file relative to the parent directory.
  Path Rel File ->
  -- | Content of the file.
  FileContent ->
  -- | IO action that writes the file content.
  IO (Either ErrorMessage ())
writeRelativeFile parentDir file content = UnliftIO.catchIO action recover
  where
    action :: IO (Either ErrorMessage ())
    action = ByteString.writeFile path bs >> pure (Right ())

    recover :: IOException -> IO (Either ErrorMessage ())
    recover ioe = pure . Left . ErrorMessage $ message
      where
        message :: ShortText
        message = "hspretty: Error writing file: " <> exceptionMessage

        exceptionMessage :: ShortText
        exceptionMessage = ShortText.pack . UnliftIO.displayException $ ioe

    bs :: ByteString
    bs = unFileContent content

    path :: FilePath
    path = relativeFilePath parentDir file

-- | Find the 'FilePath' of a file relative to a directory.
--
-- For example:
--
-- >>> :set -XQuasiQuotes
-- >>> import qualified Path
-- >>> parent = [Path.absdir|/home/someuser/project|]
-- >>> child = [Path.relfile|somedir/somefile.txt|]
-- >>> relativeFilePath parent child
-- "/home/someuser/project/somedir/somefile.txt"
relativeFilePath ::
  -- | Path of the parent directory of the file.
  Path Abs Dir ->
  -- | Path of the file relative to the parent directory.
  Path Rel File ->
  -- | FilePath of the complete, absolute file.
  FilePath
relativeFilePath parentDir file = Path.toFilePath (parentDir </> file)

-- | Convert 'FileContent' from an underlying 'ByteString' to UTF-8.
--
-- If this operation fails with a unicode error, the underlying exception is
-- converted to an 'ErrorMessage'.
fileContentToUtf8 ::
  -- | Content to convert to UTF-8.
  FileContent ->
  -- | Path to the relative file, if known (used for error messages).
  Maybe (Path Rel File) ->
  -- | Either an error message, or the file read as UTF-8 text.
  Either ErrorMessage Text
fileContentToUtf8 fileContent maybeFile =
  case Encoding.decodeUtf8' (unFileContent fileContent) of
    Right txt -> Right txt
    Left unicodeException ->
      Left . ErrorMessage . ShortText.pack $
        case maybeFile of
          Nothing ->
            "hspretty: error decoding file contents as UTF-8: "
              <> UnliftIO.displayException unicodeException
          Just file ->
            "hspretty: error decoding file \""
              <> Path.fromRelFile file
              <> "\" as UTF-8: "
              <> UnliftIO.displayException unicodeException

-- | Encode 'Text' as 'FileContent' in UTF-8.
utf8TextToFileContent ::
  -- | Text to encode.
  Text ->
  -- | Text encoded as 'FileContent'.
  FileContent
utf8TextToFileContent = FileContent . Encoding.encodeUtf8

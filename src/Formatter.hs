module Formatter where

import Data.ByteString (ByteString)
import qualified Data.ByteString as ByteString
import Data.Text.Short (ShortText)
import Path (Abs, Dir, File, Path, Rel, (</>))
import qualified Path
import RunMode (RunMode)
import qualified RunMode

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
    -- This is a pure function. For some formatters, it may be necessary to
    -- run this action using 'unsafePerformIO', but in that case, every effort
    -- should still be made to ensure it behaves as a pure function.
    Format (FileContent -> FormattingResult FileContent)

-- | Result of running a formatter.
--
-- The type parameter 'a' is the result returned when formatting has changed.
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
      content <- readRelativeFile parentDir file
      case formatFn content of
        NotFormatted -> pure NotFormatted
        Unchanged -> pure Unchanged
        Changed newContent -> do
          case runMode of
            RunMode.CheckOnly -> pure (Changed ())
            RunMode.Format -> do
              writeRelativeFile parentDir file newContent
              pure (Changed ())
        Error message -> pure (Error message)

-- | Read a relative file into 'FileContent'.
readRelativeFile ::
  -- | Path to the parent directory of the file.
  Path Abs Dir ->
  -- | Path of the file relative to the parent directory.
  Path Rel File ->
  -- | IO action containing the file content.
  IO FileContent
readRelativeFile parentDir file = FileContent <$> ByteString.readFile path
  where
    path :: FilePath
    path = relativeFilePath parentDir file

-- | Write a relative file from 'FileContent'.
writeRelativeFile ::
  -- | Path of the parent directory of the file.
  Path Abs Dir ->
  -- | Path of the file relative to the parent directory.
  Path Rel File ->
  -- | Content of the file.
  FileContent ->
  -- | IO action that writes the file content.
  IO ()
writeRelativeFile parentDir file content = ByteString.writeFile path bs
  where
    bs :: ByteString
    bs = unFileContent content

    path :: FilePath
    path = relativeFilePath parentDir file

-- | Find the 'FilePath' of a file relative to a directory.
relativeFilePath ::
  -- | Path of the parent directory of the file.
  Path Abs Dir ->
  -- | Path of the file relative to the parent directory.
  Path Rel File ->
  -- | FilePath of the complete, absolute file.
  FilePath
relativeFilePath parentDir file = Path.toFilePath (parentDir </> file)

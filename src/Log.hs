{-# LANGUAGE OverloadedStrings #-}

-- |
-- Description : Logging of CLI actions.
module Log
  ( -- * Types
    Log (..),

    -- ** Default Logger
    defaultLog,
  )
where

import qualified Data.Text as Text
import qualified Data.Text.IO
import Data.Text.Short (ShortText)
import qualified Data.Text.Short as ShortText
import Formatter (FormattingResult)
import qualified Formatter
import Path (File, Path, Rel, fromRelFile)
import qualified System.Console.ANSI as ANSI

-- | A logger.
--
--   All messages set to the user go through this interface.
data Log = Log
  { info :: ShortText -> IO (),
    report :: Path Rel File -> FormattingResult () -> IO ()
  }

-- | Default logger.
defaultLog :: Log
defaultLog =
  Log
    { info = defaultInfo,
      report = defaultReport
    }

defaultInfo :: ShortText -> IO ()
defaultInfo = Data.Text.IO.putStrLn . ShortText.toText

defaultReport :: Path Rel File -> FormattingResult () -> IO ()
defaultReport file result =
  case result of
    Formatter.NotFormatted -> pure ()
    Formatter.Unchanged -> do
      boxTick
      Data.Text.IO.putStr " - "
      putFileName file
      Data.Text.IO.putStr "\n"
    Formatter.Changed () -> do
      boxF
      Data.Text.IO.putStr " - "
      putFileName file
      Data.Text.IO.putStr "\n"
    Formatter.Error msg -> do
      boxCross
      Data.Text.IO.putStr " - "
      putFileName file
      Data.Text.IO.putStr "\n"
      Data.Text.IO.putStrLn . ShortText.toText . Formatter.unErrorMessage $ msg

putFileName :: Path Rel File -> IO ()
putFileName = Data.Text.IO.putStr . Text.pack . Path.fromRelFile

boxTick :: IO ()
boxTick = do
  Data.Text.IO.putStr "["
  ANSI.setSGR [ANSI.SetColor ANSI.Foreground ANSI.Vivid ANSI.Green]
  Data.Text.IO.putStr "✓"
  ANSI.setSGR [ANSI.Reset]
  Data.Text.IO.putStr "]"

boxF :: IO ()
boxF = do
  Data.Text.IO.putStr "["
  ANSI.setSGR [ANSI.SetColor ANSI.Foreground ANSI.Vivid ANSI.Yellow]
  Data.Text.IO.putStr "F"
  ANSI.setSGR [ANSI.Reset]
  Data.Text.IO.putStr "]"

boxCross :: IO ()
boxCross = do
  Data.Text.IO.putStr "["
  ANSI.setSGR [ANSI.SetColor ANSI.Foreground ANSI.Vivid ANSI.Red]
  Data.Text.IO.putStr "✕"
  ANSI.setSGR [ANSI.Reset]
  Data.Text.IO.putStr "]"

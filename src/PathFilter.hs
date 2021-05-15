{-# LANGUAGE OverloadedStrings #-}

-- |
-- Description : Filters for file paths.
module PathFilter where

import Data.Text.Short (ShortText)
import qualified Data.Text.Short as ShortText
import Path (Dir, File, Path, Rel)
import qualified Path

-- | Does a filter accept a path?
data PathAccept
  = -- | Filter accepts the path.
    Accept
  | -- | Filter rejects the path.
    Reject

-- | Convert a 'PathAccept' type to a 'Bool' suitable for filtering.
toBool :: PathAccept -> Bool
toBool Accept = True
toBool Reject = False

-- | Convert a 'Bool' to a 'PathAccept'.
fromBool :: Bool -> PathAccept
fromBool True = Accept
fromBool False = Reject

-- | Path filter: examing a relative file and decide if we accept it.
newtype PathFilter = PathFilter
  { unPathFilter :: Path Rel File -> PathAccept
  }

instance Semigroup PathFilter where
  f1 <> f2 = PathFilter $ \path ->
    case unPathFilter f1 path of
      Reject -> Reject
      Accept -> unPathFilter f2 path

instance Monoid PathFilter where
  mempty = PathFilter $ const Accept

-- | PathFilter that excludes hidden files or directories, starting with a
--   period.
pfNoHidden :: PathFilter
pfNoHidden = componentFilter (fromBool . ShortText.isPrefixOf ".")

-- | PathFilter that keeps only files with a given extension.
--
-- The extension to be tested should start with a period.
pfExtension :: ShortText -> PathFilter
pfExtension extension = PathFilter $ \path -> fromBool (extensionMatches path)
  where
    extensionMatches :: Path Rel File -> Bool
    extensionMatches path = extension == ext path

    ext :: Path Rel File -> ShortText
    ext path = maybe "" ShortText.pack (Path.fileExtension path)

-- | Create a filter from a function that examines each component of a path.
componentFilter :: (ShortText -> PathAccept) -> PathFilter
componentFilter f = PathFilter $ \path ->
  fromBool . not . any (toBool . f) . pathComponents $ path

-- | Return the components of a path as a list.
pathComponents :: Path Rel File -> [ShortText]
pathComponents filePath = components
  where
    components :: [ShortText]
    components = reverse (fileName : pathParts)

    fileName :: ShortText
    fileName = ShortText.pack . Path.fromRelFile . Path.filename $ filePath

    pathParts :: [ShortText]
    pathParts = go (Path.parent filePath)

    go :: Path Rel Dir -> [ShortText]
    go dir
      | isTopDir dir = []
      | otherwise = curDirComp dir : go (Path.parent dir)
      where
        isTopDir :: Path Rel Dir -> Bool
        isTopDir d = Path.fromRelDir d == "."

        curDirComp :: Path Rel Dir -> ShortText
        curDirComp d =
          ShortText.pack . init . Path.fromRelDir . Path.dirname $ d

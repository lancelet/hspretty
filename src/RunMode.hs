module RunMode
  ( RunMode (CheckOnly, Format),
  )
where

-- | Running mode: are we only checking, or also formatting?
data RunMode
  = -- | Only check files (read only).
    CheckOnly
  | -- | Format files (read and write).
    Format

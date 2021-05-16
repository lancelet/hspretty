-- |
-- Description : Main entry point for tests.
module Main where

import qualified Test.DocTest as DocTest

main :: IO ()
main = runDocTests

runDocTests :: IO ()
runDocTests = do
  putStrLn "┌───────────────────┐"
  putStrLn "│ Starting DocTests │"
  putStrLn "└───────────────────┘"
  docTests
  putStrLn "┌───────────────────┐"
  putStrLn "│ Finished DocTests │"
  putStrLn "└───────────────────┘"

docTests :: IO ()
docTests =
  DocTest.doctest
    [ "-isrc",
      "src/Formatter.hs",
      "src/PathFilter.hs"
    ]

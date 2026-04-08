module Main (main) where

import Game.Skeleton (greeting)
import System.Exit (exitFailure, exitSuccess)

main :: IO ()
main
  | null greeting = exitFailure
  | otherwise     = exitSuccess

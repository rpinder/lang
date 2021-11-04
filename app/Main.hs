module Main where

import Repl

main :: IO ()
main = do
  putStrLn "Lang REPL, :? for help"
  repl

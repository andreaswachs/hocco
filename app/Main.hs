module Main where

import Lib
import TST
import System.Environment
import System.Exit
import System.IO
import System.Directory

import Data.List


main :: IO ()
main = do
  args <- getArgs
  runProgram args
module Lib
    ( runProgram
    ) where

import System.IO
import System.Directory
import Data.List
import Text.Printf

import Args

runProgram :: [String] -> IO ()
runProgram args = do
  let flags = Args.processArgs args
  if Args.isValidConfig flags
    then do
      count <- countOccurances $ Args.getFilenameAndNeedle flags
      printf "Count: %d\n" count
    else Args.giveArgsError flags

countOccurances :: (String, String) -> IO Integer
countOccurances (filename, needle) = do
  fileExists <- doesFileExist filename
  if fileExists then do
    instreamHandle <- openFile filename ReadMode
    handleCountOccurances (filename, needle, 0) instreamHandle
  else do
    putStrLn "Error: file not found."
    return 0

handleCountOccurances :: (String, String, Integer) -> Handle -> IO Integer
handleCountOccurances args@(filename, needle, count) instreamHandle = do
  reachedEOF <- hIsEOF instreamHandle
  if reachedEOF then return count
  else do
    lineStr <- hGetLine instreamHandle
    handleLineRead args instreamHandle lineStr

handleLineRead :: (String, String, Integer) -> Handle -> String -> IO Integer
handleLineRead args@(filename, needle, count) instreamHandle line =
  let newCount = shiftyOccuranceCounter needle line count
  in handleCountOccurances (filename, needle, newCount) instreamHandle

shiftyOccuranceCounter :: String -> String -> Integer -> Integer
shiftyOccuranceCounter needle [] count = count
shiftyOccuranceCounter needle line@(_ : ls) count
  | length line < length needle = count
  | needle `isPrefixOf` line = shiftyOccuranceCounter needle ls count + 1
  | otherwise = shiftyOccuranceCounter needle ls count

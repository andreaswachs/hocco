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
    then countOccurances $ Args.getFilenameAndNeedle flags
    else Args.giveArgsError flags

-- ###################################################################
-- # Here comes the function to start the occurance count
-- ###################################################################

countOccurances :: (String, String) -> IO ()
countOccurances (filename, needle) = do
  fileExists <- doesFileExist filename
  if fileExists then do
    instreamHandle <- openFile filename ReadMode
    countOccurancesHandler (filename, needle, 0) instreamHandle
  else
    putStrLn "The file does not exists"
  
-- ###################################################################
-- # Here comes the function to keep the counting going 
-- ###################################################################

countOccurancesHandler :: (String, String, Integer) -> Handle -> IO ()
countOccurancesHandler args@(filename, needle, count) instreamHandle = do
  inEOF <- hIsEOF instreamHandle
  if inEOF then printf "Found %d occurances of the string \"%s\" in the file \"%s\"\n" count needle filename
  else do 
    lineStr <- hGetLine instreamHandle
    handleLine args instreamHandle lineStr
  return ()

-- ###################################################################
-- # here comes the function to handle each line of input
-- ###################################################################

handleLine :: (String, String, Integer) -> Handle -> String -> IO ()
handleLine args@(filename, needle, count) instreamHandle line = do
  countOccurancesHandler (filename, needle, shiftyOccuranceCounter needle line count) instreamHandle

-- ###################################################################
-- # Here comes the - shifty - function for counting occurances of a substring
-- ###################################################################

shiftyOccuranceCounter :: String -> String -> Integer -> Integer
shiftyOccuranceCounter needle [] count = count
shiftyOccuranceCounter needle line@(_ : ls) count
  | length line < length needle = count
  | needle `isPrefixOf` line = shiftyOccuranceCounter needle ls count + 1
  | otherwise = shiftyOccuranceCounter needle ls count

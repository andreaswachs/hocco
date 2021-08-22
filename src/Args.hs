module Args 
        ( Flags
        , processArgs
        , isValidConfig
        , giveArgsError
        , getFilenameAndNeedle
        ) 
where

import Data.List

-- The Flags data type is a bunch of configuration information
-- for the current run of the program
data Flags = 
    Flags {
            needle :: String
          , filename :: String
          , isFilenameSet :: Bool
          , isNeedleSet :: Bool
          , validFlags :: Bool
          } deriving Show

-- ###################################################################
-- # Here comes functions to proces args into a Flags record
-- ###################################################################

processArgs :: [String] -> Flags
processArgs args = 
    processArgsPrivate
        args 
        (Flags {filename="", needle="", isFilenameSet=False, isNeedleSet=False, validFlags = False})

processArgsPrivate :: [String] -> Flags -> Flags
processArgsPrivate [] flags = flags
processArgsPrivate (x : xs) flags =
    if "--" `isPrefixOf` x then
        -- We'll skip options for now
        processArgsPrivate xs flags
        --parseOption x xs flags
    else
        parseRequiredSetting x xs flags

parseOption :: String -> [String] -> Flags -> Flags
parseOption x xs flags =
    processArgsPrivate xs flags

parseRequiredSetting :: String -> [String] -> Flags -> Flags
parseRequiredSetting x xs flags =
    if isFilenameSet flags then
        if isNeedleSet flags then
            processArgsPrivate xs (flags {validFlags = True})
        else 
            processArgsPrivate xs (flags {needle = x, isNeedleSet = True, validFlags = True})
    else
        processArgsPrivate xs (flags {filename = x, isFilenameSet = True})

-- ###################################################################
-- # Here comes functions for validating a Flags record and report errors
-- ###################################################################

isValidConfig :: Flags -> Bool
isValidConfig = validFlags

giveArgsError flags =
    if isFilenameSet flags then
      if isNeedleSet flags then do
          putStrLn "Everything should be fine, but the program determined that an error occurred.. This is strange"
          print flags
          print "\n"
        else 
          putStrLn "The needle (string to look for) was not set. This is the second argument to the program."
      else
        putStrLn "The filename was not set. This is the first argument to the program."

-- ###################################################################
-- # Here comes functions to extract information from a Flags record
-- ###################################################################

getFilenameAndNeedle :: Flags -> (String, String)
getFilenameAndNeedle flags =
    (filename flags, needle flags)

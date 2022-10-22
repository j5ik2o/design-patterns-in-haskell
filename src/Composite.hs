{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}

module Composite where

import Data.String.Interpolate

data Entry
  = File {name :: String, size :: Int}
  | Directory {name :: String, entries :: [Entry]}
  deriving (Show)

getName :: Entry -> String
getName File {name, ..} = name
getName Directory {name, ..} = name

getSize :: Entry -> Int
getSize File {size, ..} = size
getSize Directory {entries, ..} = sum $ map getSize entries

display :: Entry -> String
display File {name, size} = [i|#{name} (#{size})|]
display Directory {name, entries} = [i|#{name} (#{sum $ map getSize entries})|]

printLine :: Entry -> IO ()
printLine entry = printLineWithPrefix entry ""

printLineWithPrefix :: Entry -> String -> IO ()
printLineWithPrefix d@Directory {name, entries} prefix = do
  putStrLn [i|#{prefix}/#{display d}|]
  mapM_ (`printLineWithPrefix` [i|#{prefix}/#{name}|]) entries
printLineWithPrefix f@File {..} prefix = putStrLn [i|#{prefix}/#{display f}|]

addEntry :: Entry -> Entry -> Entry
addEntry Directory {entries, ..} entry = Directory {entries = entry : entries, ..}
addEntry _ _ = error "addEntry: first argument must be a directory"

ofDir :: String -> Entry
ofDir name = Directory {name, entries = []}

ofFile :: String -> Int -> Entry
ofFile name size = File {name, size}

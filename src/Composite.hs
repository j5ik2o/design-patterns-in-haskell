{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}

module Composite where

import Data.String.Interpolate

data Entry
  = File String Int
  | Directory String [Entry]
  deriving (Show)

getName :: Entry -> String
getName (File name _) = name
getName (Directory name _) = name

getSize :: Entry -> Int
getSize (File _ size) = size
getSize (Directory _ entries) = sum $ map getSize entries

display :: Entry -> String
display (File name size) = [i|#{name} (#{size})|]
display (Directory name entries) = [i|#{name} (#{s})|]
 where
   s = sum $ map getSize entries

printLine :: Entry -> IO ()
printLine entry = printLineWithPrefix entry ""

printLineWithPrefix :: Entry -> String -> IO ()
printLineWithPrefix d@(Directory name entries) prefix = do
  putStrLn [i|#{prefix}/#{display d}|]
  mapM_ (`printLineWithPrefix` [i|#{prefix}/#{name}|]) entries
printLineWithPrefix f@(File _ _) prefix = putStrLn [i|#{prefix}/#{display f}|]

addEntry :: Entry -> Entry -> Entry
addEntry (Directory name entries) entry = Directory name $ entry : entries
addEntry _ _ = error "addEntry: first argument must be a directory"

ofDir :: String -> Entry
ofDir name = Directory name []

ofFile :: String -> Int -> Entry
ofFile = File

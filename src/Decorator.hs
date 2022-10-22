{-# LANGUAGE QuasiQuotes #-}
module Decorator where

import Data.String.Interpolate

data Display = String String | SideBorder Display Char | FullBorder Display

getColumns :: Display -> Int
getColumns (String s) = length s
getColumns (SideBorder display _) = 1 + getColumns display + 1
getColumns (FullBorder display) = 1 + getColumns display + 1

makeLine :: Char -> Int -> String
makeLine char count = replicate count char

getRows :: Display -> Int
getRows (String s) = 1
getRows (SideBorder display c) = getRows display
getRows (FullBorder display) = 1 + getRows display + 1

getRowText :: Display -> Int -> String
getRowText (String text) row
  | row == 0 = text
  | otherwise = error "Invalid row"
getRowText (SideBorder display char) row = [i|#{bracket}#{message}#{bracket}|]
  where
    bracket = [char]
    message = getRowText display row
getRowText (FullBorder display) row
  | row == 0 = [i|-#{message}-|]
  | row == getRows display + 1 = [i|+#{message}+|]
  | otherwise = [i||#{line}||]
  where
    message = makeLine '-' $ getColumns display
    line = getRowText display $ row - 1

show :: Display -> IO ()
show display = mapM_ putStrLn $ map (getRowText display) [0..getRows display - 1]
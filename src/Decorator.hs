{-# LANGUAGE QuasiQuotes #-}
module Decorator where

import Data.String.Interpolate

data Display = String String | SideBorder Display Char | FullBorder Display

getColumns :: Display -> Int
getColumns (String s) = length s
getColumns (SideBorder d c) = 1 + getColumns d + 1
getColumns (FullBorder d) = 1 + getColumns d + 1

makeLine :: Char -> Int -> String
makeLine c n = replicate n c

getRows :: Display -> Int
getRows (String s) = 1
getRows (SideBorder d c) = getRows d
getRows (FullBorder d) = 1 + getRows d + 1

getRowText :: Display -> Int -> String
getRowText (String s) row
  | row == 0 = s
  | otherwise = error "Invalid row"
getRowText (SideBorder d c) row = [i|#{bracket}#{message}#{bracket}|]
  where
    bracket = [c]
    message = getRowText d row
getRowText (FullBorder d) row
  | row == 0 = [i|-#{message}-|]
  | row == getRows d + 1 = [i|+#{message}+|]
  | otherwise = [i||#{line}||]
  where
    message = makeLine '-' (getColumns d)
    line = getRowText d $ row - 1

show :: Display -> IO ()
show d = mapM_ putStrLn $ map (getRowText d) [0..getRows d - 1]
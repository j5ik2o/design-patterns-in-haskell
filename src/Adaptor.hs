{-# LANGUAGE QuasiQuotes #-}

module Adaptor where

import Data.String.Interpolate

newtype Banner = Banner {value :: String}

showWithParen :: Banner -> IO ()
showWithParen (Banner name) = putStrLn [i|(#{name})|]

showWithAster :: Banner -> IO ()
showWithAster (Banner name) = putStrLn [i|*#{name}*|]

class Print a where
  printWeak :: a -> IO ()
  printStrong :: a -> IO ()

newtype PrintBanner = PrintBanner {banner :: Banner}

instance Print PrintBanner where
  printWeak (PrintBanner b) = showWithParen b
  printStrong (PrintBanner b) = showWithAster b

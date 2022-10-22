module Adaptor where

newtype Banner = Banner {value :: String}

showWithParen :: Banner -> IO ()
showWithParen (Banner v) = putStrLn $ "(" ++ v ++ ")"

showWithAster :: Banner -> IO ()
showWithAster (Banner v) = putStrLn $ "*" ++ v ++ "*"

class Print a where
  printWeak :: a -> IO ()
  printStrong :: a -> IO ()

newtype PrintBanner = PrintBanner {banner :: Banner}

instance Print PrintBanner where
  printWeak (PrintBanner b) = showWithParen b
  printStrong (PrintBanner b) = showWithAster b

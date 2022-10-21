module Command where

class Command a where
  execute :: a -> IO ()

newtype MacroCommand a = MacroCommand [a]

instance Command a => Command (MacroCommand a) where
  execute (MacroCommand commands) = mapM_ execute commands

append :: Command a => MacroCommand a -> a -> MacroCommand a
append (MacroCommand commands) command = MacroCommand (commands ++ [command])

undo :: Command a => MacroCommand a -> MacroCommand a
undo (MacroCommand commands) = MacroCommand (init commands)

clear :: MacroCommand a -> MacroCommand a
clear _ = MacroCommand []

newtype EchoCommand = EchoCommand String

instance Command EchoCommand where
  execute (EchoCommand message) = putStrLn message
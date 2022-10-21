module Command where

data Command = EchoCommand String | MacroCommand [Command]

execute :: Command -> IO ()
execute (EchoCommand message) = putStrLn message
execute (MacroCommand commands) = mapM_ execute commands

append :: Command -> Command -> Command
append f@(EchoCommand _) s@(EchoCommand _) = MacroCommand [f, s]
append f@(EchoCommand _) (MacroCommand commands) = MacroCommand $ f : commands
append (MacroCommand commands) s@(EchoCommand _) = MacroCommand $ commands ++ [s]
append (MacroCommand l) (MacroCommand r) = MacroCommand $ l ++ r

undo :: Command -> Command
undo (MacroCommand commands) = MacroCommand (init commands)

clear :: Command -> Command
clear _ = MacroCommand []
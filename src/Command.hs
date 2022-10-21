module Command where

data Command = EchoCommand String | MacroCommand [Command]

execute :: Command -> IO ()
execute (EchoCommand message) = putStrLn message
execute (MacroCommand commands) = mapM_ execute commands

append :: Command -> Command -> Command
append (MacroCommand commands) command = MacroCommand (commands ++ [command])

undo :: Command -> Command
undo (MacroCommand commands) = MacroCommand (init commands)

clear :: Command -> Command
clear _ = MacroCommand []
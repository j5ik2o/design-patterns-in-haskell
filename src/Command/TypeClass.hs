module Command.TypeClass where

class Command a where
  execute :: a -> IO ()

newtype EchoCommand = EchoCommand String
newtype MacroCommand a = MacroCommand [a]

instance Command EchoCommand where
  execute (EchoCommand s) = putStrLn s

instance (Command a) => Command (MacroCommand a) where
  execute (MacroCommand xs) = mapM_ execute xs

instance Semigroup (MacroCommand a) where
  (MacroCommand xs) <> (MacroCommand ys) = MacroCommand (xs <> ys) 
  
instance Monoid (MacroCommand a) where
  mempty = MacroCommand []
  mappend (MacroCommand xs) (MacroCommand ys) = MacroCommand (xs ++ ys)
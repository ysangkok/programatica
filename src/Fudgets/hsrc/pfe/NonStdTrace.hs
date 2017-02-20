module NonStdTrace where

foreign import trace :: String -> a -> a

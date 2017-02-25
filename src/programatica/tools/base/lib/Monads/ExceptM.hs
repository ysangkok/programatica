module ExceptM (module ExceptM, HasExcept(..)) where

import MT
import Control_Monad_Fix

import Control.Monad (liftM)


type ExceptM          = Either

removeExcept          :: ExceptM x a -> Either x a
removeExcept          = id

mapEither f g         = either (Left . f) (Right . g)
seqEither x           = either (fmap Left) (fmap Right) x

fromEither f (Right x)= x
fromEither f (Left x) = f x

unLeft (Left x)       = x
unLeft _              = error "unLeft"

unRight (Right x)     = x
unRight _             = error "unRight"

instance Functor (Either x) where
  fmap                = liftM

instance Monad (Either x) where
  return              = Right
  Right x >>= f       = f x
  Left x >>= f        = Left x

  Right _ >> m        = m
  Left x >> m         = Left x
    

instance HasExcept (Either x) x where
  raise               = Left
  handle h (Left x)   = h x
  handle h (Right x)  = Right x
  

instance MonadFix (Either x) where
  mfix f              = let a = f (unRight a) in a

instance HasBaseMonad (Either x) (Either x) where
  inBase              = id    



module Ap where

infixl 9 `ap`
infixl 1 #,<#

( # )  :: (Functor f) => (a->b) -> f a -> f b
(<#) :: Monad f => f (a->b) -> f a -> f b

( # ) = fmap -- doesn't work with GHC
(<#) = ap
-- Eqn: f # x = return f <# x

ap :: Monad m => m (a -> b) -> m a -> m b
f `ap` a = do fv <- f; av <- a; return (fv av)

#ifdef __GLASGOW_HASKELL__
-- from hbc_library1.3/MonadUtil:
{-
map_             :: (Monad m) => (a -> m b) -> ([a] -> m ())
map_ f []        = return ()
map_ f (x:xs)    = f x >> map_ f xs
-}

foldR            :: (Monad m) => (a -> b -> m b) -> b -> [a] -> m b
foldR f a []     = return a
foldR f a (x:xs) = foldR f a xs >>= \y -> f x y
#endif

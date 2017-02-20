module NameMonad where

infixl 5 `apN`

type NameMonad a = Int-> (a,Int)

unitN :: a -> NameMonad a
unitN x s = (x,s)

bindN :: NameMonad a -> (a->NameMonad b) -> NameMonad b
xm `bindN` f = \ s -> case xm s of
                       (x,s) -> f x s

getN :: NameMonad Int
getN s = (s,s+1)

apN :: NameMonad (a->b) -> NameMonad a -> NameMonad b
fm `apN` xm = fm `bindN` \ f -> xm `bindN` \ x -> unitN (f x)

mapN :: (a->b) -> NameMonad a -> NameMonad b
mapN f ma = unitN f `apN` ma

binN :: (a->b->c) -> NameMonad a -> NameMonad b -> NameMonad c
binN f ma mb = unitN f `apN` ma `apN` mb

maprN :: (a->NameMonad b)->[a]->NameMonad [b]
maprN _  []     = unitN []
maprN fm (x:xs) = binN (:) (fm x) (maprN fm xs)

concatMaprN :: (a->NameMonad [b])->[a]->NameMonad [b]
concatMaprN _  []     = unitN []
concatMaprN fm (x:xs) = binN (++) (fm x) (concatMaprN fm xs)

doN :: Int -> NameMonad a -> a
doN n m = fst (m n)

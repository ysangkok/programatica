{-# COMPILERFLAGS -fpbu #-}
module SwapMonad(SwapMonad,run,swap,smap) where

#if __HASKELL1__==5
#define map fmap
#endif

run :: SwapMonad a b -> [a] -> (b,[a])
swap :: a -> SwapMonad a a


newtype SwapMonad a b = M ([a]->[a]->(b,[a],[a]))

run (M sm) is0 =
  let (y,is1,os1) = sm is0 []
  in (y,os1)

swap x = M $ \ (i:is) os -> (i,is,x:os)


instance Monad (SwapMonad a) where
  return x = M $ \ is os -> (x,is,os)

  M sm1 >>= xsm2 =
     M $ \ is0 os3 ->
       let (x,is1,os1) = sm1 is0 os2
           M sm2 = xsm2 x
           (y,is2,os2) = sm2 is1 os3
       in (y,is2,os1)

instance Functor (SwapMonad a) where
  map f m =
    do x <- m
       let y = f x
       return y
       --seq y $ return y

#if __HASKELL1__==5
smap :: (a->b) -> SwapMonad m a -> SwapMonad m b
#else
smap :: (Eval b) => (a->b) -> SwapMonad m a -> SwapMonad m b
#endif
smap f m =
    do x <- m
       let y = f x
       seq y $ return y

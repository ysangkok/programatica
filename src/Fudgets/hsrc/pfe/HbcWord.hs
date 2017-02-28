module HbcWord where

data Word = Word Int
data Short
data Byte

class Bits a where
  bitAnd,bitOr,bitXor :: a -> a -> a
  bitCompl,bitSwap :: a -> a
  bitRsh,bitLsh :: a -> Int -> a
  bit0 :: a
  bitSize :: a -> Int

instance Num Word
instance Bits Word
instance Ord Word
instance Eq Word
instance Show Word

wordToInt (Word n) = n

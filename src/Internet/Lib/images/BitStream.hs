module BitStream where
import Trace(trace)

type Byte = Int
data BitStream = End | Zero BitStream | One BitStream
end = End

bitstream :: [Byte] -> BitStream

bitstream [] = End
bitstream (b:bs) = bits 8 b (bitstream bs)
  where
    bits :: Int->Int->BitStream->BitStream
    bits 0 b bs = bs
    bits n b bs = if b `mod` 2 /=0 -- the function odd is inefficient
                  then One (bits (n-1) (b `div` 2) bs)
		  else Zero (bits (n-1) (b `div` 2) bs)

btake eos s =
  btake' (const eos .  trace ("Bitstream.btake: expected "++show s
                              {-++" bits, but got only "++show (s-n)-}))
         s

btake' :: (Int->Int) -> Int -> BitStream -> (Int,BitStream)
btake' eos s = g s 1 0
  where
    g 0 e a bs = (a,bs)
    g n e a (Zero bs) = g (n-1) (2*e) a bs
    g n e a (One bs) = g (n-1) (2*e) (a+e) bs
    g n _ a End = (eos a,end)

bytestream :: BitStream -> [Byte]
bytestream End = []
bytestream bs =
  case btake' id 8 bs of
    (b,bs) -> b:bytestream bs

bput :: Int->Int->BitStream->BitStream
bput 0 n bs = bs
bput cnt n bs =
  if odd n
  then One  (bput (cnt-1) (n `div` 2) bs)
  else Zero (bput (cnt-1) (n `div` 2) bs)


{- -- Simple version:
type BitStream = [Bool]
end = []
bitstream = concatMap bits

bits :: Int->[Bool]
bits = b 8
  where b 0   n = []
        b cnt n = odd n:b (cnt-1) (n `div` 2)

int :: [Bool] -> Int
int = int' 1
  where
    int' e [] = 0
    int' e (b:bs) = fromEnum b*e+int' (2*e) bs
-}

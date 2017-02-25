module Base64(encodeBase64,decodeBase64) where
import ListUtil(chopList)
import Data.Array
import Utils2(chr,ord)
-- Contents-Transfer-Encoding Base64

-- #define PAD	64

encodeBase64 = concatMap e64 . chopList (splitAt ((76 `div` 4)*3))

decodeBase64 = d64 . map (decodetab !) . filter isinalphabet
	-- should warn about strange chars...

e64 [] = "\n"
e64 [c] = encode1 c ++ "\n"
e64 [c1,c2] = encode2 c1 c2 ++ "\n"
e64 (c1:c2:c3:cs) = encode3 c1 c2 c3 ++ e64 cs

encode3 c1 c2 c3 = itos64 4 (ord c3+256*(ord c2+256*ord c1))

encode2 c1 c2 = take 3 (encode3 c1 c2 '\0') ++ "="

encode1 c1 = take 2 (encode3 c1 '\0' '\0') ++ "=="

itos64 0 n = []
itos64 k n = let d= encodetab ! (n `mod` 64)
                 ds=itos64 (k-1) (n `div` 64)
	     in ds++[d]

d64 [] = []
d64 [c1,c2,64,64] = decode2 c1 c2
d64 [c1,c2,c3 ,64] = decode3 c1 c2 c3
d64 (c1:c2:c3:c4:cs) = decode4 c1 c2 c3 c4 ++ d64 cs
d64 cs = [] --fail (show_list itos cs)

decode4 c1 c2 c3 c4 = itos256 3 (((c1*64+c2)*64+c3)*64+c4)

--decode4 c1 c2 c3 c4 = itos256 3 (c4+64*(c3+64*(c2+64*c1))) -- out of D-regs !!

decode3 c1 c2 c3 = take 2 (decode4 c1 c2 c3 0)

decode2 c1 c2 = take 1 (decode4 c1 c2 0 0)

itos256 0 n = []
itos256 k n = let d=chr(n `mod` 256)
 		  ds=itos256 (k-1) (n `div` 256)
	      in ds++[d]

alphabet = "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/"
isinalphabet c = 'A'<=c && c<='Z' || 'a'<=c && c<='z' || '0'<=c && c<='9' || 
                 c=='+' || c=='/' || c=='='

encodetab = listArray (0::Int,63) alphabet

decodetab = accumArray (const id) 64 (chr 0,chr 255)
                       (zip alphabet [0..63::Int])

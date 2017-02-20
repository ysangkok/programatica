module Mixfix(Mixfix,Permutation,groupArgs,mixfixArity,isDelimited,permutation,permute,invperm,mix) where
import Char(isSpace,isDigit,digitToInt)
import List(sort,groupBy)

newtype Permutation = P [Int]
newtype Mixfix = M [String]
unM (M ss) = ss

groupArgs = M . groupBy argplace
  where argplace c1 c2 = not (isArgChar c1 || isArgChar c2)

--mixfixArity = length . filter isArg . unM
mixfixArity (P []) = 0
mixfixArity (P is) = 1+maximum is

isArgChar c = c=='_' || isDigit c

isArg [c] = isArgChar c
isArg _ = False

isDelimited (M ss@(s:_)) = delim s && delim (last ss)
  where delim s = not (isArg s || all isSpace s)

permutation (M ss) = P $ perm ss [0..]
  where perm [] is = []
	perm ([c]:ss) is | isArgChar c =
	  case c of
	    '_' -> case is of
		     i:is -> i:perm ss is
	    d | isDigit d && i `elem` is ->
		i:perm ss (filter (/=i) is)
	      where i=digitToInt d - 1
	    _ -> error ("DrawAlf.permutation "++show ss)
	perm (_:ss) is = perm ss is

permute es (P is) = map (es!!) is

invperm (P is) = P . map snd . sort $ zip is [0..]

mix (M ss) = mix' ss
  where
    mix' (s:ss) es =
      if isArg s
      then case es of
	     e:es -> Right e:mix' ss es
	     _ -> error ("DrawAlf.mix bug: "++show (s:ss))
      else Left s:mix' ss es
    mix' [] [] = []
    mix' ss es = error ("DrawAlf.mix bug: "++show ss++" "++show es)

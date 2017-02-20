-- Lexical analysis for GF files
-- (to speed up parsing to provide acurate source positions in syntax
-- error reports)
module LexGrammar(
  GTokenClass(..),GToken,PToken(..),kkk,sss,iii,nnn,
  glexpos,ptStr,
  LexPos(..),
  reservedGFWords,
  remComments
  ) where
import Char(isDigit,isAlpha,isAlphaNum,isSpace)
import List(mapAccumL)
import HO(apFst)
--import Trace(trace)

import LexPos

data GTokenClass
  = I -- identifier
  | N -- numeric literal
  | G -- GF string literal, ``blaha''
  | S -- string literal, "blaha"
  | K -- keyword, or special character
  | E -- lexical error
  | W -- white space, removed in a separate pass
  deriving (Eq,Show)

kkk = K --- to be able to hide K
sss = S
iii = I
nnn = N

type GToken = (GTokenClass,String)
data PToken = PT !Pos GToken deriving (Show)

ptStr (PT _ (_,s)) = s

instance Eq PToken where
  PT _ t1 == PT _ t2 = t1==t2

instance LexPos PToken where
  lexPos (PT p _) = p
  lexShow = unwords . map ptStr . take 10  -- a quick hack

glexpos = filter notJunk . addpos . glex
  where
    addpos = snd . mapAccumL pos startPos

    pos p t@(k,s) = seq p' (p',PT p t)
      where p' = nextPos p s

    notJunk (PT _ (W,_)) = False
    notJunk _ = True

glex [] = []
glex s@('-':'-':_) = case break (=='\n') s of
		       (cmnt,rest) -> (W,cmnt):glex rest
glex ('{':'-':cs) = case nestedComment cs of
	              (cmnt,rest) -> (W,'{':'-':cmnt):glex rest
glex ('`':'`':cs) = case gfString cs of
	              (cmnt,rest) -> (G,'`':'`':cmnt):glex rest
glex s@(c:cs) =
  case c of
    '"'           -> cspanTok [c]              cs
    _ | isDigit c -> kspan N    isDigit     c cs
      | isAlpha c -> fspan word isAlphaNum_ c cs
      | isSpace c -> kspan W    isSpace     c cs
      | otherwise -> (K,[c]):glex cs

kspan k = fspan ((,) k)
fspan f p c cs =
  case span p cs of
    (cs1,rest) -> f (c:cs1) : glex rest

cspanTok w cs = -- AR 26/2/2001 for escapes
  case cs of
    '\\':'"' :cs2 -> cspanTok ('"':w) cs2
    '\\':'\\':cs2 -> cspanTok ('\\':w) cs2
    '"'      :cs2 -> (S,reverse ('"':w)) : glex cs2 
    c        :cs2 -> cspanTok (c:w) cs2
    []            -> [(S, reverse w)]

cspan k c cs =
  case span (/=c) cs of
    (cs1,c1:rest) -> (k,c:cs1++[c1]) : glex rest
    (cs1,[]) -> [(k,cs1)] -- EOF inside string!!

word s =
  if s `elem` reservedGFWords
  then (K,s)
  else (I,s)

nestedComment [] = ([],[]) -- EOF inside comment!
nestedComment ('-':'}':rest) = ("-}",rest)
nestedComment (c:cs) = apFst (c:) (nestedComment cs)

gfString [] = ([],[]) -- EOF inside GF string!
gfString ('\'':'\'':rest) = ("''",rest)
gfString (c:cs) = apFst (c:) (gfString cs)

isAlphaNum_ c = c=='_' || c=='\'' || isAlphaNum c

reservedGFWords = 
  ["Lin", "Str", "Strs","Type",
   "cat","category","def","fun","in","include","let","lin","lincat","lintype","of",
   "oper","param","pattern","post", "pre","rule","strs","table","tokenizer","type",
   "var","case","lindef", "printname",
   "grammar", "concrete", "abstract", "sig", "struct", "data" -- not actual
  ]

-- comment removal : line tails prefixed by -- as well as chunks in {- ... -}
remComments :: String -> String
remComments s = 
  case s of
    '"':s2 -> '"':pass remComments s2 -- comment marks in quotes are not removed!
    '{':'-':cs -> readNested cs
    '-':'-':cs -> readTail cs
    c:cs -> c : remComments cs
    [] -> []
   where
     readNested t = 
       case t of
         '"':s2 -> '"':pass readNested s2
         '-':'}':cs -> remComments cs
         _:cs -> readNested cs
         [] -> []
     readTail t = 
       case t of
         '\n':cs -> '\n':remComments cs
         _:cs -> readTail cs
         [] -> []
     pass f t = 
       case t of
         '"':s2 -> '"': f s2
         c:s2 -> c:pass f s2
         _ -> t

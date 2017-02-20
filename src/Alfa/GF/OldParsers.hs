module Parsers where

import Operations

infixr 2 |||
infixr 2 +||
infixr 5 .>.
infixr 5 ...
infixr 5 ....
infixr 5 +..
infixr 5 ..+
infixr 3 ***
infixr 3 *?*
infixr 6 |>
infixr 3 <<<

-- parser combinators a` la Wadler and Hutton. AR 1998 -- 21/9/1999
-- Copyright (c) Aarne Ranta 1998--1999, under GNU General Public License (see GPL)

type Parser a b = [a] -> [(b,[a])]

succeed :: b -> Parser a b
succeed v s = [(v,s)]

fails :: Parser a b
fails s = []

(|||) :: Parser a b -> Parser a b -> Parser a b
(p1 ||| p2) s = p1 s ++ p2 s

(.>.) :: Parser a b -> (b -> Parser a c) -> Parser a c
(p1 .>. p2) s = [(b,z) | (a,y) <- p1 s, (b,z) <- p2 a y]

(...) :: Parser a b -> Parser a c -> Parser a (b,c)
p1 ... p2 = p1 .>. (\x -> p2 .>. (\y -> succeed (x,y)))

(+..) :: Parser a b -> Parser a c -> Parser a c
p1 +.. p2 = p1 .>. (\x -> p2 .>. (\y -> succeed y))

(..+) :: Parser a b -> Parser a c -> Parser a b
p1 ..+ p2 = p1 .>. (\x -> p2 .>. (\y -> succeed x))

(***) :: Parser a b -> (b -> c) -> Parser a c
p *** f = p .>. (\x -> succeed (f x))

(*?*) :: Parser a b -> (b -> Maybe c) -> Parser a c
p *?* f = p .>. (\x -> case f x of Just c -> succeed c
                                   _      -> fails)

(<<<) :: Parser a b -> c -> Parser a c  -- return
p <<< v = p *** (\x -> v)

item :: Parser a a
item [] = []
item (a:x) = [(a,x)]

(|>) :: Parser a b -> (b -> Bool) -> Parser a b
p |> b = p .>. (\x -> if b x then succeed x else fails)

satisfy :: (a -> Bool) -> Parser a a
satisfy b = item |> b

literal :: (Eq a) => a -> Parser a a
literal x = satisfy (==x)

literOpt :: String -> Parser String String
literOpt [] = succeed []
literOpt s  = literal s

literNet :: Net Char -> Parser String String
literNet net = satisfy (acceptByNet net)

first :: Parser a b -> Parser a b
first p s = case p s of []    -> []
                        (x:l) -> [x]

(+||) :: Parser a b -> Parser a b -> Parser a b
p1 +|| p2 = first (p1 ||| p2)

many :: Parser a b -> Parser a [b]
many p = p .>. (\x -> many p .>. (\y -> succeed (x:y))) ||| succeed []

some :: Parser a b -> Parser a [b]
some p = (p ... many p) *** (\ (x,y) -> x:y)

longestOfMany :: Parser a b -> Parser a [b]
longestOfMany p = 
  guarantee 
   (p .>. (\x -> longestOfMany p .>. (\y -> succeed (x:y))) +|| succeed [])

pLongestIdent :: [String] -> Parser Char String
pLongestIdent cc = pIdent |> (\x -> elem x cc)

guarantee :: Parser a b -> Parser a b
guarantee p s = let u = p s in (fst (head u),snd (head u)) : tail u

closure :: (b -> Parser a b) -> (b -> Parser a b)
closure p v = p v .>. closure p ||| succeed v

pJunk   :: Parser Char String
pJunk = longestOfMany (satisfy (\x -> elem x "\n\t "))

pJ :: Parser Char a -> Parser Char a
pJ p = pJunk +.. p ..+ pJunk

pTList  :: String -> Parser Char a -> Parser Char [a]
pTList t p = p .... many (jL t +.. p) *** (\ (x,y) -> x:y) ---- mod. AR 5/1/1999

pTJList  :: String -> String -> Parser Char a -> Parser Char [a]
pTJList t1 t2 p = p .... many (literals t1 +.. jL t2 +.. p) *** (\ (x,y) -> x:y)

pLookup :: [(String,a)] -> Parser Char a
pLookup cc = pLongestIdent (map fst cc) *** (\i -> case lookup i cc of Just y -> y)
--foldl (+||) fails [literals s *** (\x -> c) | (s,c) <- cc]

pElem   :: [String] -> Parser Char String
pElem l = foldl (+||) fails (map literals l)

(....) :: Parser Char b -> Parser Char c -> Parser Char (b,c)
p1 .... p2 = p1 ... pJunk +.. p2

literals :: (Eq a) => [a] -> Parser a [a]
literals l = case l of []  -> succeed [] 
                       a:l -> literal a ... literals l *** (\ (x,y) -> x:y)

pOpt :: (Eq a) => [a] -> Parser a [a]
pOpt l = succeed [] ||| literals l

lits :: (Eq a) => [a] -> Parser a [a]
lits  = literals

jL :: String -> Parser Char String
jL = pJ . lits

jLL :: String -> Parser Char String
jLL s = appends (words s) where
 appends []     = succeed ""
 appends [w]    = jL w
 appends (w:ww) = jL w ... appends ww *** (\ (t,tt) -> t +++ tt)

pParenth p = literal '(' +.. pJunk +.. p ..+ pJunk ..+ literal ')'
pCommaList p = pTList "," (pJ p)                      -- p,...,p
pOptCommaList p = pCommaList p ||| succeed []            -- the same or nothing
pArgList p = pParenth (pCommaList p) ||| succeed [] -- (p,...,p), poss. empty

longestOfSome p = (p ... longestOfMany p) *** (\ (x,y) -> x:y)

pLongestMatch :: [String] -> Parser Char String
pLongestMatch strings = pElem (sortByLongest strings)

pReplicate int parser = 
 case int of
   0 -> succeed []
   _ -> parser ... pReplicate (int - 1) parser *** (\ (x,y) -> x:y)

pIdent         = longestOfSome (satisfy (`elem` alphaPlusChars))
alphanum       = satisfy (`elem` alphanumChars) -- for compatibility
alphanumChars  = ['0'..'9'] ++ ['A'..'Z'] ++ ['a'..'z']
alphaPlusChars = alphanumChars ++ ['_','\''] ++ ['à' .. 'û']

pFileName =
  longestOfSome (satisfy (`elem` nameChar))
   where nameChar = ['0'..'9'] ++ ['A'..'Z'] ++ ['a'..'z'] ++ "/._-"

pQuotedString = literal '"' +.. (\s -> [endQuoted s]) where
 endQuoted s =
  case s of
   '\\':'n':t -> ('\n' : w, u) where (w,u) = endQuoted t 
   '\\':'t':t -> ('\t' : w, u) where (w,u) = endQuoted t 
   '\\':c:t   -> (c : w, u) where (w,u) = endQuoted t 
   '"':t      -> ([],t)
   c:t        -> (c : w, u) where (w,u) = endQuoted t
   []         -> error "unterminated string"

pVarIdent cc = pIdent |> (\x -> not (elem x cc)) 
-- exclude elements of C from var idents

pInt :: Parser String Int
pInt = satisfy (all numb) *** read
         where numb x = elem x ['0'..'9']

pIntc :: Parser Char Int
pIntc = some (satisfy numb) *** read
         where numb x = elem x ['0'..'9']

fullParses p = map fst (filter ((==[]) . snd) p)

optField s p = optlitt s .... p *** snd ||| succeed []

optlitt :: String -> Parser Char String
optlitt s = literals s +|| literals (take 3 s) -- +|| succeed ""

pEitherOrder :: Parser a b -> Parser a c -> Parser a (b,c)
pEitherOrder p q = (p ... q) ||| (q ... p) *** (\ (x,y) -> (y,x))

chooseFirstParse :: Parser a b -> [a] -> b -> b
chooseFirstParse parser input deflt =
 case parser input of
   (x,[]):_ -> x
   _        -> deflt


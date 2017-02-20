module Parsers(module ParserType,module Parsers) where

import Operations((+++),sortByLongest)
import ParserType

import Char

infixr 5 ...
infixr 5 ....
infixr 5 +..
infixr 5 ..+
infixr 3 *?*
infixr 6 |>
infixr 3 <<<

(...) :: Parser a b -> Parser a c -> Parser a (b,c)
p1 ... p2 = p1 .>. (\x -> p2 *** (\y -> (x,y)))

(+..) :: Parser a b -> Parser a c -> Parser a c
p1 +.. p2 = p1 .>. const p2

(..+) :: Parser a b -> Parser a c -> Parser a b
p1 ..+ p2 = p1 .>. (\x -> p2 *** (const x))

(*?*) :: Parser a b -> (b -> Maybe c) -> Parser a c
p *?* f = p .>. (\x -> case f x of Just c -> succeed c
                                   _      -> fails "< *?* >")

(<<<) :: Parser a b -> c -> Parser a c  -- return
p <<< v = p *** (\x -> v)

(|>) :: Parser a b -> (b -> Bool) -> Parser a b
p |> b = p .>. (\x -> if b x then succeed x else fails "??")

{- --unused?
literOpt :: String -> Parser String String
literOpt [] = succeed []
literOpt s  = literal s
-}

--literNet :: Net Char -> Parser String String
--literNet net = satisfy (acceptByNet net)

many :: Parser a b -> Parser a [b]
many p = p .>. (\x -> many p *** (x:)) ||| succeed []

some :: Parser a b -> Parser a [b]
some p = (p ... many p) *** (\ (x,y) -> x:y)

manyPre pre p = longestOfMany (pre +.. p)
someSep sep p = (p ... manyPre sep p) *** uncurry (:)
manySep sep p = someSep sep p +|| succeed []

longestOfMany :: Parser a b -> Parser a [b]
longestOfMany p = p .>. (\x -> longestOfMany p *** (x:)) +|| succeed []

pLongestIdent :: [String] -> Parser Char String
pLongestIdent cc = pIdent |> (\x -> elem x cc)

closure :: (b -> Parser a b) -> (b -> Parser a b)
closure p v = p v .>. closure p ||| succeed v

pJunk   :: Parser Char String
--pJunk = longestOfMany (satisfy (\x -> elem x "\n\t "))
pJunk = satisfyspan (`elem` "\n\t ")

pJ :: Parser Char a -> Parser Char a
pJ p = pJunk +.. p ..+ pJunk

pTList  :: String -> Parser Char a -> Parser Char [a]
pTList t p = p .... many (jL t +.. p) *** (\ (x,y) -> x:y) ---- mod. AR 5/1/1999

pTJList  :: String -> String -> Parser Char a -> Parser Char [a]
pTJList t1 t2 p = p .... many (literals t1 +.. jL t2 +.. p) *** (\ (x,y) -> x:y)

pLookup :: [(String,a)] -> Parser Char a
pLookup cc = pLongestIdent (map fst cc) *** (\i -> case lookup i cc of Just y -> y)

pElem   :: [String] -> Parser Char String
pElem l = foldr (+||) (fails (unwords l)) (map literals l)

(....) :: Parser Char b -> Parser Char c -> Parser Char (b,c)
p1 .... p2 = p1 ... pJunk +.. p2

--satisfy :: (a -> Bool) -> Parser a a
satisfy b = item |> b

--literal :: (Eq a) => a -> Parser a a
literal x = satisfy (==x) +|| fails (show x)

--literals :: (Eq a,Show a) => [a] -> Parser a [a]
literals l = case l of []  -> succeed [] 
                       a:l -> literal a ... literals l *** (\ (x,y) -> x:y)

--pOpt :: (Eq a,Show a) => [a] -> Parser a [a]
pOpt l = succeed [] ||| literals l

--lits :: (Eq a,Show a) => [a] -> Parser a [a]
lits ts = literals ts

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
pArgList2 p = pParenth (p ... jL "," +.. pCommaList p) *** uncurry (:) -- min.2 args

longestOfSome p = (p ... longestOfMany p) *** (\ (x,y) -> x:y)

pLongestMatch :: [String] -> Parser Char String
pLongestMatch strings = pElem (sortByLongest strings)

pReplicate int parser = 
 case int of
   0 -> succeed []
   _ -> parser ... pReplicate (int - 1) parser *** (\ (x,y) -> x:y)

--pIdent = pLetter ... longestOfMany pAlphaPlusChar *** uncurry (:)
pIdent =
    (satisfy isAlpha ... satisfyspan alphaPlusChar *** uncurry (:))
    +|| fails "<pIdent>"
  where alphaPlusChar c = isAlphaNum c || c=='_' || c=='\''

pLetter        = satisfy (`elem` (['A'..'Z'] ++ ['a'..'z'] ++ 
                                  ['À' .. 'Û'] ++ ['à' .. 'û'])) -- no such in Char
pDigit         = satisfy isDigit
pLetters       = longestOfSome pLetter
pAlphanum      = pDigit ||| pLetter
pAlphaPlusChar = pAlphanum ||| satisfy (`elem` "_'")

pFileName =
  longestOfSome (satisfy (`elem` nameChar))
   where nameChar = ['0'..'9'] ++ ['A'..'Z'] ++ ['a'..'z'] ++ "/._-~"


pQuotedString = literal '"' +.. pEndQuoted where
 pEndQuoted =
       literal '"' *** (const [])
   +|| (literal '\\' +.. item .>. \ c -> pEndQuoted *** (c:))
   +|| item .>. \ c -> pEndQuoted *** (c:)


{- --old:
pQuotedString = literal '"' +.. wadlerParser endQuoted where
 endQuoted s = case s of
   '\\':c:t   -> [(c : w, u) | (w,u) <- endQuoted t] 
   '"':t      -> [([],t)]
   c:t        -> [(c : w, u) | (w,u) <- endQuoted t]
   []         -> []
--}

pVarIdent cc = pIdent |> (\x -> not (elem x cc)) +|| fails "<varIdent>"
-- exclude elements of C from var idents

{- -- unused?
pInt :: Parser String Int
pInt = satisfy (all numb) *** read
         where numb x = elem x ['0'..'9']
-}

pIntc :: Parser Char Int
pIntc = some (satisfy numb) *** read
         where numb x = elem x ['0'..'9']

fullParses p = map fst (filter ((==[]) . snd) p)

optField s p = optlitt s .... p *** snd ||| succeed []

optlitt :: String -> Parser Char String
optlitt s = literals s +|| literals (take 3 s) -- +|| succeed ""

pEitherOrder :: Parser a b -> Parser a c -> Parser a (b,c)
pEitherOrder p q = (p ... q) ||| (q ... p) *** (\ (x,y) -> (y,x))

{- -- unused? Belongs somwhere else.
chooseFirstParse :: Parser a b -> [a] -> b -> b
chooseFirstParse parser input deflt =
 case parses parser input of
   (x,[]):_ -> x
   _        -> deflt
-}

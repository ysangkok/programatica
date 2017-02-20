module AlfParserMonad(
    Parse,         -- Parser monad type, abstract
    --ErrorPos(..),
    parse,parse',      -- run a parser on some input
    unitP,             -- monad unit, parses the empty string (always succeeds)
    mapP,	       -- map
    bindP, apP,        -- standard monad operations, parse sequences
    thenP, chkP,       -- parse sequences, throw away one part
    orP,eitherP,       -- parses alternatives
    manyP,someP,       -- repetition parsers (zero or more, resp. one or more)
    optionalP,maybeP,  -- parser for optional items (zero or one)
    checkP, symbolP,   -- parsers that accept one terminal symbol
    idenP,             -- parser that accepts an identifier
    conP,              -- parser that accepts a constructor name
    numP,              -- parser that accepts a number (sequence of digits)
    commentP,             -- parser that accepts a comment
    --doG,               -- lift G monads to Parse monads
    errorP,            -- parser that fails (not recommended)
    theRestP)          -- returns the remaining token stream
                       -- (e.g., to be able to split parsing into two stages)
    where
import AlfLex
import Char(isDigit)
import List(nub)

infixr 0 `orP`
infixl 1 `bindP`,`thenP`
infixl 2 `apP`,`chkP`

--type Parse a = [TOKEN] -> G (a,[TOKEN]) -- version 0

newtype Parse a = P (ErrorPos -> [PosTOKEN] -> (ErrorPos,E (a,[PosTOKEN])))
unP (P p) = p

type ErrorPos = (Pos,TOKEN,[String]) -- high water mark

type E a = Either String a
elimE e f g = either g f e
unitE = Right
errorE = Left

--showErrPos :: (Pos,String) -> String
showErrPos (pos,msg) = "At " ++ showPos pos ++": "++msg
  where
    showPos pos@(line,column) =
      if pos==eotPos
      then "end of file"
      else unwords ["Line",show line,"column",show column]
  
--showErrPos' :: ErrorPos -> (Pos,String)
showErrPos' (p,t,msgs) = 
    (p, if p==eotPos
        then unwords ("expected":expected)
	else unwords ("got":string_of_token t: "but expected": expected))
  where
    expected =
      case nub msgs of
	[] -> ["nothing!?"] -- this can't happen
	[e] -> [e]
	es -> "one of":es

--parse :: Parse a -> [PosTOKEN] -> G a
parse p tkl = 
  case parse' p tkl of
    Left err -> Left (showErrPos err)
    Right a -> Right a

--parse' :: Parse a -> [PosTOKEN] -> Either (Pos,String) a
parse' p tkl =
  let (ep,e) = unP (p `chkP` eofP) ((0,0),Symbol "BOT",[]) tkl
  in elimE e (\(a,tkl) ->
               case tkl of
	         [] -> Right a
		 _  -> Left (showErrPos' ep))
	     (\_->Left (showErrPos' ep))

--unitP :: a -> Parse a
unitP a = P $ \ ep tkl -> (ep,unitE (a,tkl))

--bindP :: Parse a -> (a->Parse b) -> Parse b
P p1 `bindP` xp2 =
   P $ \ ep tkl ->
   let (ep1,e1) = p1 ep tkl
   in elimE e1 (\(x,tkl2)->unP (xp2 x) ep1 tkl2)
               (\s->(ep1,errorE s))

p1 `thenP` p2 = p1 `bindP` const p2

--apP :: Parse (a->b) -> Parse a -> Parse b
fp `apP` xp = fp `bindP` \ f -> xp `bindP` \ x -> unitP (f x)

--mapP :: (a->b) -> Parse a -> Parse b
f `mapP` xp = xp `bindP` \ x -> unitP (f x)

--chkP :: Parse a -> Parse b -> Parse a
p1 `chkP` p2 = unitP const `apP` p1 `apP` p2

--doG :: G a -> Parse a
--doG g ep tkl = g `bindG` \ x -> unitP x ep tkl

--errorP :: String -> Parse a
errorP = P . errorP'

errorP' msg ep [] = (updErr ep (eot,msg),errorE msg) -- hmm...
errorP' msg ep tkl@(pt:_) = (updErr ep (pt,msg),errorE msg)

--updErr :: ErrorPos -> (PosTOKEN,String) -> ErrorPos
updErr e1@(p1,t1,msgs) ((p2,t2),msg) =
    if p1==p2
    then (p1,t1,msg:msgs)
    else if p2<p1
	 then e1
	 else (p2,t2,[msg])

--orP :: Parse a -> Parse a -> Parse a
P p1 `orP` P p2 = P $ \ ep tkl ->
  let (ep1,e1) = p1 ep tkl
  in elimE e1 (\xtkl1-> (ep1,unitE xtkl1))
	      (\s-> let (ep2,e2) = p2 ep1 tkl
	            in elimE e2 (\xtkl2-> (ep2,unitE xtkl2))
			        (\s->(ep2,errorE s)))

pl `eitherP` pr = mapP Left pl `orP` mapP Right pr

optionalP x p = p `orP` unitP x
maybeP p = optionalP Nothing (mapP Just p)

manyP p = optionalP [] (someP p)
someP p = unitP (:) `apP` p `apP` manyP p

{-
--tokenP :: Parse TOKEN
tokenP ep [] = errorG "unexpected end of file"
tokenP ep ((p,t):tkl) = unitG (ep,unitE (t,tkl))
-}

--theRestP :: Parse [PosTOKEN]
theRestP = P $ \ ep tkl -> (ep,unitE (tkl,[eot]))

--peekP :: Parse TOKEN
peekP = P peekP'
  where
    peekP' ep tkl@(pt@(_,t):_) = (ep,unitE (t,tkl))
    peekP' ep tkl = errorP' "Unexpected end of file" ep tkl

--skipP :: a -> Parse a
skipP x = P $ \ ep (_:tkl) -> (ep,unitE (x,tkl))

eofP = checkP (Symbol "EOT")

checkP t =
  peekP `bindP` \ t' ->
  if t'==t
  then skipP ()
  else errorP (string_of_token t)

symbolP = checkP . Symbol

idenP =
  peekP `bindP` \ t ->
  case t of
    Iden s -> skipP s
    _ -> errorP ("<identifier>")

conP =
  peekP `bindP` \ t ->
  case t of
    Con s -> skipP s
    _ -> errorP ("<constructor>")

numP =
  peekP `bindP` \ t ->
  case t of
    Iden s | all isDigit s -> skipP (read s::Int)
    _ -> errorP "<number>"

commentP =
  peekP `bindP` \ t ->
  case t of
    Comment' s -> skipP s
    _ -> errorP ("<comment>")

--
{-
--predictP :: Parse b -> (b->Parse a) -> Parse a -> Parse a
predictP p0 p1 p2 tkl =
  caseG (p0 tkl)
        (\(x,tkl2)-> p1 x tkl2)
	(p2 tkl)
-}

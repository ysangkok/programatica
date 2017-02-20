module OldParsOps(Parser,Error(..),parse,tstParser,unit,fail,bind,ap,chk,orelse,some,many,seq,lit,scan,tok,cap,maybe,optional,Either..) where

infixl 3 `ap`,`chk`,`bind`, `cap`
infixl 2 `orelse`

-- "Parser a b" is a parser that parses a sequence of "a":s into a "b".
data Parser a b = P ([a]->Either (Error a) (b,[a]))

-- An error message contains a descriptive message and the remaining input.
type Error a = (String, [a])

{-# MACRO err #-}
err (msg,xs) = Left (msg, xs)

parse (P p) xs =
  case p xs of
    Right (y,[])  -> Right y
    Right (_,xs)  -> err ("garbage after end",xs)
    Left  err     -> Left err

tstParser (P p) xs = p xs

unP (P p) = p

fail msg = P (\ xs -> err (msg,xs))

-- unit is the "unit" operation of the monad. It parses an empty string.
unit y = P (\xs -> Right (y,xs))

-- Bind is the "bind" operations of the monad.

bind (P p1) yp2 =
  P (\xs -> case p1 xs of
              Left errxs -> Left errxs
              Right (y,xs') -> unP (yp2 y) xs')

p1 `ap` p2 = p1 `bind` \ f -> p2 `bind` \ x -> unit (f x)
p1 `chk` p2 = p1 `bind` \ x -> p2 `bind` \ _ -> unit x

bp `cap` cp = unit id `chk` bp `ap` cp


(P p1) `orelse` (P p2) =
  P (\xs -> case p1 xs of
	      Right yxs -> Right yxs
	      Left  _   -> p2 xs)

seq = many

many p = some p `orelse` unit []
some p = unit (:) `ap` p `ap` many p

lit f = P (\xs -> case xs of
		    []      -> err ("unexpected end of input",[])
		    (x:xs') -> case f x of
				 Just y -> Right (y,xs')
				 Nothing -> err ("unexpected token",xs))

scan p = lit (\t -> if p t then Just t else Nothing)
tok t = scan (t==)

optional x p = p `orelse` unit x

maybe p = unit Just `ap` p `orelse` unit Nothing

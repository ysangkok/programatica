{-# COMPILERFLAGS -fno-overload-restr #-}
module ParsOps(module ParseLib,module ParsOps,
  -- bug workaround line:
  some,ap,chk,orelse,many,eof,bind,Parser(..),ParseGood(..),ParseError(..),ParseBad(..),ParseResult(..),Pos,apCut,manySep,someSep
  ) where
import qualified ParseLib as NR
import ParseLib hiding (literal, parse , parseAp , token , revChk )
			  
import Pos

tok x=NR.literal x
unit x=NR.parse x
mapP x=NR.parseAp x
nrtoken x=NR.token x
cap x=NR.revChk x

-- An error message contains a list of expected input and the remaining input.
type Error a = ([String], [a])

parse :: Parser (Pos i) a i a -> [i] -> Either (Error i) a
parse p = conv .parseit p . addPos
  where
    conv (Left (pos,got,expected)) = Left (expected,snd(getPos pos))
    conv (Right x) = Right x

token = nrtoken $ \ _ t -> Right t

scan p = nrtoken $ \ _ t -> if p t then Right t else Left "unexpected token"

lit f = nrtoken $ \ _ t ->
        case f t of
	  Just y  -> Right y
	  Nothing -> Left "unexpected token" -- push t back on remaining input.

litP = nrtoken . const

failP :: x -> Parser p a i c
failP s = parseFail -- !!

toks [] = unit []
toks (t:ts) = (:) `mapP` tok t `ap` toks ts

optional x p = p `orelse` unit x

maybeP p = Just `mapP` p `orelse` unit Nothing

p `filterP` f = p `bind` \a->if f a then unit a else failP "filterP failed"

module AIdent(
 AIdent,
 varToGF,consToGF,constrToGF,
 varFromGF,consFromGF,constrFromGF,
 isConstrId,
 idFromGF,
 pAIdent
 ) where

{-
This module defines how identifiers introduced by the Alfa user are
represented in the GF grammars and terms generated from Alfa documents.
To avoid name clashes, the following naming conventions are used:

  + Alfa constants are prefixed with AA (e.g.: Nat -> AANat)
    (manipulated by functions named consXXX).

  + Alfa constructors are prefixed with CC (e.g.: Zero@_ -> CCZero)
    (manipulated by functions named constrXXX).

  + Symbolic constants and constructors are translated with a AA0 or CC0
    prefix, followed by a hexadecimal encoding of the symbolic characters.

  + Alfa variables (lambda-bound identifiers) are kept unchanged (since they
    are represented as string literals in the GF grammar and can thus be
    distinguished from identifiers.) This means that nothing special has to be
    done when linearizing terms containing lambda-bound variables.
    (manipulated by functions named varXXX), and that variable names can be
    symbolic, without any special encoding.

  + The core grammar does not use names beginning with AA or CC.
-}

import qualified AlfaPluginKit as A
import qualified GSyntax as G
import qualified Grammar as G
import qualified Operations as G
import Parsers(pIdent,pJ,literals,succeed,(+..),(...),(|||),(<<<),(***),
	       longestOfSome,satisfy,Parser)
import Lex(isSym) -- from Agda
import Char(isAlpha,chr,ord)
import Numeric(readHex)
import Debug2(trace)

type AIdent = String -- SIdent

--- To be used primarily in A2GSyntax:
varToGF :: A.Var -> G.GString
varToGF (A.Var s) = G.GString s

--- To be used in A2GSyntax and AlfatoGrammar:
consToGF :: A.Var -> AIdent
constrToGF :: A.Con -> AIdent

consToGF (A.Var s) = "AA"++encode s
constrToGF (A.Con s) = "CC"++encode s

-- Used by the two-pass translation in AlfaToGrammar, and in G2ASyntax:
isConstrId :: AIdent -> Bool
isConstrId ('C':'C':_) = True
isConstrId _ = False

--- To be used in G2ASyntax:
varFromGF :: G.GString -> A.Var
consFromGF :: AIdent -> A.Var
constrFromGF :: AIdent -> A.Con

varFromGF (G.GString s) = A.Var s
consFromGF ('A':'A':s) = A.Var (decode s)
consFromGF s = trace ("consFromGF "++s) (A.Var s) -- Should not happen!!

constrFromGF ('C':'C':s) = A.Con (decode s)
constrFromGF s = trace ("constrFromGF "++s) (A.Con s) -- Should not happen!!

--- Just for for pretty printing grammatical annotations:
idFromGF :: AIdent -> String
idFromGF ('A':'A':s) = decode s -- constants
idFromGF ('C':'C':s) = decode s++"@_" -- constructors
idFromGF s = s -- variables

--- To be used when parsing annotations from Alfa files  (in PATheory)
--pAIdent :: G.Parser Char AIdent
pAIdent =
    pJ pIdent' ... ((literals "@_" <<< "CC")|||succeed "AA")
    *** uncurry enc
  where
    enc s prefix = prefix++encode s
    pIdent' = pIdent ||| pSymbolicIdent
    pSymbolicIdent = longestOfSome (satisfy isSym)

---
encode s@(c:_) = if c=='_' || isAlpha c then s else '0':encodeSymbols s

decode ('0':s) = decodeSymbols s
decode s = s

encodeSymbols = concatMap encodeSymbol
  where
    encodeSymbol c = hex (ord c)
    hex n = [hex1 (n `div` 16),hex1 (n `mod` 16)] -- Doesn't allow Unicode!!
    hex1 n = "0123456789abcdef"!!n -- slow!

decodeSymbols (d1:d2:ds) =
  case readHex [d1,d2] of
    (n,_):_ -> chr n:decodeSymbols ds
    _ -> d1:d2:ds
decodeSymbols s = s

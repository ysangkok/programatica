module GSyntax where

import Types
import PrTypes (prTerm)

-- syntax for Agda-GF interface. AR 11/11/1999 -- 19/11
-- most of this code was generated automatically from the GF grammar
-- by the program GrammarToHaskell

-- first the hand-written code

data GString = GString String

class Gf a where gf :: a -> Term
class Fg a where fg :: Term -> a

instance Gf GString where gf (GString s) = Predef ("String",s)

instance Fg GString where 
 fg t = 
  case termForm t of
    ([],Predef ("String",s),[]) -> GString s
    _ -> error ("no GString " ++ prTerm t)

appc :: String -> [Term] -> Term
appc s tt = apps (Cons (Fun s)) tt

data GBsElem = GBsElem String [GExpr]
data GBsSet  = GBsSet  String [GExpr]

instance Gf GBsElem where
 gf (GBsElem c ee) = appc c (map fromExpr ee) --deviation from pattern

instance Gf GBsSet where
 gf (GBsSet c ee) = appc c (map fromExpr ee) --deviation from pattern

instance Fg GBsElem where
 fg t =
  case termForm t of
    ([], Cons (Fun f), xx) -> GBsElem f (map fg xx)
    _ -> error ("no BsElem " ++ prTerm t)

instance Fg GBsSet where
 fg t =
  case termForm t of
    ([], Cons (Fun f), xx) -> GBsSet f (map fg xx)
    _ -> error ("no BsElem " ++ prTerm t)

fromExpr :: GExpr -> Term 
fromExpr (GelemExpr e) = gf e
fromExpr (GsetExpr  e) = gf e


-- then the generated code

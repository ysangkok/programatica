module GSyntax where

import Grammar
import PrGrammar ---(prt)
import Macros
import Operations

-- syntax for Agda-GF interface. AR 11/11/1999 -- 6/3/2000
-- most of this code was generated automatically from the GF grammar
-- by the program GrammarToHaskell

-- first the hand-written code

data GString = GString String

class Gf a where gf :: a -> Trm
class Fg a where fg :: Trm -> a

instance Gf GString where gf (GString s) = Literal s

instance Fg GString where 
 fg t = 
  case termForm t of
    Ok ([], Literal s, []) -> GString s
    _ -> error ("no GString" +++ prt t)

appc :: String -> [Trm] -> Trm
appc s tt = mkApp (Cons (zIdent s)) tt

data GCons = GCons String [GExp] | GVarApp GString GListExp

instance Gf GCons where 
  gf (GCons c ee) = appc c (map gf ee) --deviation from pattern
  gf (GVarApp s ee) = appc "VarApp" [gf s, gf ee]

instance Fg GCons where
  fg t =
    case termForm t of
      Ok ([], Cons (Ident ("VarApp",_)),[x1,x2]) -> GVarApp (fg x1) (fg x2)
      Ok ([], Cons f, xx) -> GCons (symid f) (map fg xx)
      _ -> error ("no Cons " ++ prt t)


-- then the generated code


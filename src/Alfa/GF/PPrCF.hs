module PPrCF where

import Operations
import CF
import Tokens
import Grammar

-- printing and parsing cf grammars AR 26/1/2000 -- 28/2

type Aa = Str    -- the token type
type Cc = String -- the identifier type
-- making this polymorphic would be cute but complicates parsing of cf files

prCF :: (Token a, CFIdent c, CFIdent f) => CF a c f -> String
prCF = unlines . (map prCFRule) . fst -- hiding the literal recognition function

prCFTree :: (CFIdent c, CFIdent f) => CFTree c f -> String
prCFTree (CFTree (fun, (_,trees))) = prCFIdent fun ++ prs trees where 
 prs [] = ""
 prs ts = " " ++ unwords (map ps ts)
 ps t@(CFTree (_,(_,[]))) = prCFTree t
 ps t = prParenth (prCFTree t)

prCFRule :: (Token a, CFIdent c, CFIdent f) => CFRule a c f -> String
prCFRule (fun,(cat,its)) = 
     prCFIdent fun ++ "." +++ prCFIdent cat +++ "->" +++ unwords (map prCFItem its)

prCFItem (CFNonterm c) = prCFIdent c
prCFItem (CFTerm a) = prRegExp a

prRegExp (RegAlts tt) = case tt of
  [t] -> prTok t
  _ -> prParenth (prTList " | " (map prTok tt))

-- rules have an amazingly easy parser, if we use the format
-- fun. C -> item1 item2 ... where unquoted items are treated as cats

getCFRule :: String -> Maybe (CFRule Aa Cc Cc)
getCFRule s = getcf (words s) where
 getcf ww | length ww > 2 && ww !! 2 == "->" = 
                        Just ((init fun), (cat, map mkIt its)) where
  fun : cat : _ : its = words s
  mkIt ('"':w@(_:_)) = mkCFTerm (Str (init w))
  mkIt w = CFNonterm w
 getcf _ = Nothing


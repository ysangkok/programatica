module GrammarToHaskell where

import Operations
import Types
import Grammar
import IOGrammar (grammarFromFiles)

infixr 5 +++++

-- to write a GF grammar into a Haskell module with translations from
-- data objects into GF trees. Example: GSyntax for Agda. AR 11/11/1999

updGS = testG2H "agdaEng.gf" "GSyntaxPreamble.hs" "GSyntax.hs"

testG2H file hwname newfile =
 do (g,_) <- grammarFromFiles emptyGrammar [file]
    hw <- readFile hwname
    let h = grammar2haskell g hw
    putStr h
    writeFile newfile h

grammar2haskell :: Grammar -> String -> String
grammar2haskell gr hw = 
 hw +++++ datatypes gr' +++++ gfinstances gr' +++++ fginstances gr' 
  where gr' = hSkeleton gr

type HSkeleton = [(Ident, [(Ident, [Ident])])]

datatypes, gfinstances, fginstances :: HSkeleton -> String
datatypes   = (foldr (+++++) "") . (filter (/="")) . (map hDatatype)
gfinstances = (foldr (+++++) "") . (filter (/="")) . (map hInstance)
fginstances = (foldr (+++++) "") . (filter (/="")) . (map fInstance)

hDatatype, hInstance, fInstance :: (Ident, [(Ident, [Ident])]) -> String
hDatatype (c,_) | take 2 c == "Bs" = "" ---
hDatatype (cat,[]) = ""
hDatatype (cat,rules) =
 "data" +++ gId cat +++ "=" ++
 (if length rules == 1 then "" else "\n  ") +++
 foldr1 (\x y -> x ++ "\n |" +++ y) 
        [gId f +++ foldr (+++) "" (map gId xx) | (f,xx) <- rules]
hInstance (c,_) | take 2 c == "Bs" = "" ---
hInstance (cat,[]) = ""
hInstance (cat,rules) = 
 "instance Gf" +++ gId cat +++ "where" ++
 (if length rules == 1 then "" else "\n") +++
 foldr1 (\x y -> x ++ "\n" +++ y) [mkInst f xx | (f,xx) <- rules]
   where
    mkInst f xx =
     "gf " ++
     (if length xx == 0 then gId f else prParenth (gId f +++ foldr1 (+++) xx')) +++
     "=" +++
     "appc" +++ "\"" ++ f ++ "\"" +++ 
     "[" ++ prTList ", " ["gf" +++ x | x <- xx'] ++ "]"
       where xx' = ["x" ++ show i | (_,i) <- zip xx [1..]]
fInstance (c,_) | take 2 c == "Bs" = "" ---
fInstance (cat,[]) = ""
fInstance (cat,rules) =
 "instance Fg" +++ gId cat +++ "where" ++++
 " fg t =" ++++
 "  case termForm t of" ++++
 foldr1 (\x y -> x ++ "\n" ++ y) [mkInst f xx | (f,xx) <- rules] ++++
 "    _ -> error (\"no" +++ cat ++ " \" ++ prTerm t)"
   where
    mkInst f xx =
     "    ([], Cons (Fun \"" ++ f ++ "\")," ++
     "[" ++ prTList "," xx' ++ "])" +++
     "->" +++
     "G" ++ f +++  
     prTList " " [prParenth ("fg" +++ x) | x <- xx']
       where xx' = ["x" ++ show i | (_,i) <- zip xx [1..]]

hSkeleton :: Grammar -> HSkeleton
hSkeleton gr@(_,(cats,_,rules,_)) = 
 map completeR (collectR rules [(c,[]) | (c,_) <- cats]) where
  collectR rr hh =
   case rr of
     (fun,(typ,_)):rs -> let (xx0,Cat cat,_) = typeForm typ
                             xx = [c | Cat c <- map (valCat . snd) xx0]
                         in collectR rs (updateSkeleton cat hh (fun,xx))
     _ -> hh
---  completeR (cat,[]) = (cat,[(cat,["String"])]) -- typically provisory 
  completeR crr      = crr

updateSkeleton :: Ident -> HSkeleton -> (Ident, [Ident]) -> HSkeleton
updateSkeleton cat skel rule =
 case skel of
   (cat0,rules):rr | cat0 == cat -> (cat0, rule:rules) : rr
   (cat0,rules):rr               -> (cat0, rules) : updateSkeleton cat rr rule

(+++++) :: String -> String -> String
x +++++ y = x ++ "\n\n" ++ y

gId :: Ident -> Ident
gId i = 'G':i


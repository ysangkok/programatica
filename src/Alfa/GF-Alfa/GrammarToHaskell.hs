module GrammarToHaskell where

import Operations
import Grammar
import Macros
import IOGrammar (readGrammarFile)

-- to write a GF grammar into a Haskell module with translations from
-- data objects into GF trees. Example: GSyntax for Agda. AR 11/11/1999 -- 3/3/2000

updGS = testG2H "alfaAbs.gfn" "gfalfa/GSyntaxPreamble.hs" "gfalfa/GSyntax.hs"

type OIdent = String

testG2H :: String -> String -> String -> IO ()
testG2H file hwname newfile =
 do g <- (readGrammarFile file :: IO (Grammar [Str]))
    hw <- readFile hwname
    let h = grammar2haskell g hw
    putStr h
    writeFile newfile h

grammar2haskell :: Token a => Grammar a -> String -> String
grammar2haskell gr hw = 
 hw +++++ datatypes gr' +++++ gfinstances gr' +++++ fginstances gr' 
  where gr' = hSkeleton gr

type HSkeleton = [(OIdent, [(OIdent, [OIdent])])]

datatypes, gfinstances, fginstances :: HSkeleton -> String
datatypes   = (foldr (+++++) "") . (filter (/="")) . (map hDatatype)
gfinstances = (foldr (+++++) "") . (filter (/="")) . (map hInstance)
fginstances = (foldr (+++++) "") . (filter (/="")) . (map fInstance)

hDatatype, hInstance, fInstance :: (OIdent, [(OIdent, [OIdent])]) -> String

hDatatype ("Cons",_) = "" ---
hDatatype (cat,[]) = ""
hDatatype (cat,rules) =
 "data" +++ gId cat +++ "=" ++
 (if length rules == 1 then "" else "\n  ") +++
 foldr1 (\x y -> x ++ "\n |" +++ y) 
        [gId f +++ foldr (+++) "" (map gId xx) | (f,xx) <- rules]

hInstance ("Cons",_) = "" ---
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

fInstance ("Cons",_) = "" ---
fInstance (cat,[]) = ""
fInstance (cat,rules) =
 "instance Fg" +++ gId cat +++ "where" ++++
 " fg t =" ++++
 "  case termForm t of" ++++
 foldr1 (\x y -> x ++ "\n" ++ y) [mkInst f xx | (f,xx) <- rules] ++++
 "    _ -> error (\"no" +++ cat ++ " \" ++ prt t)"
   where
    mkInst f xx =
     "    Ok ([], Cons (Ident (\"" ++ f ++ "\",_))," ++
     "[" ++ prTList "," xx' ++ "])" +++
     "->" +++
     "G" ++ f +++  
     prTList " " [prParenth ("fg" +++ x) | x <- xx']
       where xx' = ["x" ++ show i | (_,i) <- zip xx [1..]]

hSkeleton :: Grammar a -> HSkeleton
hSkeleton gr@(Grammar (Abstract defs,_)) = collectR rules [(c,[]) | c <- cats] where
  collectR rr hh =
   case rr of
     (fun,typ):rs -> case catSkeleton typ of
        Ok (cats,cat) -> 
             collectR rs (updateSkeleton (symid cat) hh (fun, map symid cats))
        _ -> collectR rs hh
     _ -> hh
  cats =  [symid cat | DefCat cat _ <- defs]
  rules = [(symid fun, typ) | DefFun fun typ <- defs]

updateSkeleton :: OIdent -> HSkeleton -> (OIdent, [OIdent]) -> HSkeleton
updateSkeleton cat skel rule =
 case skel of
   (cat0,rules):rr | cat0 == cat -> (cat0, rule:rules) : rr
   (cat0,rules):rr               -> (cat0, rules) : updateSkeleton cat rr rule

gId :: OIdent -> OIdent
gId i = 'G':i


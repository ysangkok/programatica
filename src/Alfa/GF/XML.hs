module XML where

import Parsers
import Operations
import Tokens

-- a fragment of XML that makes sense in GF. AR 4/1/1999 -- 5/6
-- Copyright (c) Aarne Ranta 1999, under GNU General Public License (see GPL)

-- an XML document is a DTD (Document Type Definition) followed by an XML element
type DocX = (DTD,ElemX)
type DTD  = [RuleX]
type XIdent = String

data TypeX = 
   EmptyX 
 | ArgSeqX ArgX  deriving (Show, Eq)

data ArgX  = 
   PCDataX 
 | AtomX   TagX
 | ProdX   Int  TagX
 | SeqX    ArgX ArgX 
 | AltX    ArgX ArgX 
 | StarX   ArgX 
 | PlusX   ArgX 
 | OptX    ArgX  deriving (Show, Eq)

data ElemX = 
   AppX    TagX [ElemX] 
 | AbsX    [XIdent] ElemX
 | VarX    (TagX,XIdent)
 | MetaX   (TagX,XIdent)
 | PredefX (XIdent,String)
  deriving (Show, Eq)

data TagX  = 
   TagX String  deriving (Show, Eq)

data RuleX = 
   ElemX    TagX TypeX 
 | EntitX   String String 
 | AttlistX TagX [XIdent]
  deriving (Show, Eq)

-- parsing XML documents

pDocX :: Parser Char DocX
pDocX =
 (pPrologX ||| succeed [])     .>. (\_ ->
 pJ (pDTDlocal)                .>. (\ (t,dtd) ->
 pJ (pElemX dtd (TagX t))      .>. (\tree ->
 succeed (dtd,tree))))

pPrologX  = lits "<?xml" +.. longestOfMany (satisfy (/='?')) ..+ lits "?>"

pDTDlocal = 
 lits "<!DOCTYPE" .>. (\_ -> 
 pJ pTagXIdentX    .>. (\t ->
 jL "["           .>. (\_ ->
 pDTD             .>. (\dtd ->
 jL "]>"          .>. (\_ ->
 succeed (t,dtd))))))

pDTD :: Parser Char DTD
pDTD =
 pRuleX                               .>. (\rule ->
 pJ (pDTD +|| succeed [])             .>. (\dtd' ->
 succeed (rule : dtd')))

pDTDfile = (pPrologX ||| succeed []) +.. pJ pDTDlocal *** snd ||| pDTD 

pTagXIdentX :: Parser Char String
pTagXIdentX = pXIdent |> (/= "AbsX") --- to be modified

pTypeX :: Parser Char TypeX
pTypeX =
  lits "EMPTY" <<< EmptyX
 |||
  pArgX 2 *** ArgSeqX

pArgX :: Int -> Parser Char ArgX
pArgX 3 =
  lits "#PCDATA"                   <<< PCDataX
 |||
  pTagXIdentX                       *** AtomX . TagX
 |||
  longestOfSome (jL "AbsX,") ... pTagXIdentX 
                                   *** (\ (k,t) -> ProdX (length k) (TagX t))
 |||
  pParenth (pArgX 1)
pArgX 2 =
  pArgX 3 ... pPostfX              *** (\ (a,f) -> f a)
pArgX 1 =
  pArgX 2 ... 
  (jL "," +.. pTList "," (pArgX 2) *** (\y -> \x -> (foldr1 SeqX (x:y)))
    |||
   jL "|" +.. pTList "|" (pArgX 2) *** (\y -> \x -> (foldr1 AltX (x:y)))
    +||
   succeed []                      *** (\y -> id))
    *** (\ (x,f) -> f x)

pPostfX :: Parser Char (ArgX -> ArgX)
pPostfX =
  literal '*' <<< StarX
 |||
  literal '+' <<< PlusX
 |||
  literal '?' <<< OptX
 |||
  succeed id

pFullElemX :: DTD -> Parser Char ElemX
pFullElemX dtd = remainingInput .>. pElemX dtd . findFirstTag where
 findFirstTag s = TagX (takeWhile (`notElem` " />")
                                  (dropWhile (`elem` " \n\t<") s))

pElemX :: DTD -> TagX -> Parser Char ElemX
pElemX dtd tag@(TagX t) = 
 (pStartTagX tag <<< tag) .... pAttrX "MetaX" "symb" ..+ pEndTagX tag *** MetaX
  |||
 (pStartTagX tag <<< tag) .... pAttrX "VarX"  "symb" ..+ pEndTagX tag *** VarX
  |||
 case lookupTypeX dtd tag of
   EmptyX -> 
     literal '<' +.. lits t  ..+ pJunk ..+ lits "/>" <<< AppX tag []
   ArgSeqX PCDataX ->
     pStartTagX tag               .>. (\_      -> 
     pPCDataX                     .>. (\string -> 
     pEndTagX tag                 .>. (\_      -> 
     succeed (PredefX (t,string)))))
   ArgSeqX args ->
     (pStartTagX tag)             .>. (\tag   -> 
     pJ (pElemsX dtd args)        .>. (\trees -> 
     pEndTagX tag                 .>. (\_     -> 
     succeed (AppX tag trees))))

pAttrX typ attr = 
  literal '<' +.. jL typ +.. jL attr +.. jL "=" +.. pQuotedString ..+ jL "/>"

lookupTypeX :: DTD -> TagX -> TypeX
lookupTypeX dtd (TagX t) =
 case lookup t [(t,typ) | ElemX (TagX t) typ <- dtd] of Just typ -> typ
                                                        _        -> EmptyX

pElemsX :: DTD -> ArgX -> Parser Char [ElemX]
pElemsX dtd args = 
 case args of
   PCDataX        -> pPCDataX *** (\s -> [PredefX ("String",s)])
   AtomX tag      -> pElemX dtd tag *** (:[])
   ProdX int tag  -> pAbssX int .... pElemX dtd tag *** (\ (xx,b) -> [AbsX xx b])
   SeqX arg1 arg2 -> pElemsX dtd arg1 .... pElemsX dtd arg2 *** (\ (x,y) -> x++y)
   AltX arg1 arg2 -> pElemsX dtd arg1 ||| pElemsX dtd arg2
   StarX arg      -> longestOfMany (pJ (pElemsX dtd arg)) *** foldl (++) []
   PlusX arg      -> longestOfSome (pJ (pElemsX dtd arg)) *** foldl (++) []
   OptX  arg      -> pElemsX dtd arg +|| succeed []
  where
   pAbssX int = pReplicate int (pJ (pAttrX "AbsX" "symb"))

pRuleX :: Parser Char RuleX
pRuleX =
  lits "<!ELEMENT" +.. pJ pXIdent ... pJ pTypeX ..+ literal '>'
   *** (\ (t,typ) -> ElemX (TagX t) typ)
 |||
  (lits "<!ENTITY" +.. pJ pXIdent)    .>. (\ident ->
  pJ (literal '\'' ||| literal '"')  .>. (\sep ->  --- both ' and '"' accepted !
  longestOfSome (satisfy (/= sep))   .>. (\def ->
  (literal sep ... jL ">")           .>. (\_ ->
  succeed (EntitX ident def))))) 
 |||
  lits "<!ATTLIST" +.. pJ pXIdent ... 
  longestOfMany (pJ pXIdent ..+ jL "CDATA" ..+ jL "#REQUIRED") ..+ literal '>'
   *** (\ (t,l) -> AttlistX (TagX t) l)

remSpaceX :: String -> String
remSpaceX = unwords . words ---

pStartTagX :: TagX -> Parser Char TagX
pStartTagX (TagX str) = literals "<" +.. lits str ..+ literal '>' *** TagX

pEndTagX :: TagX -> Parser Char TagX
pEndTagX (TagX str) = literals "</" +.. lits str ..+ literal '>' *** TagX

pPCDataX :: Parser Char String
pPCDataX = longestOfMany (satisfy (\x -> not (elem x "<>"))) *** (unwords . words)

pXIdent = pIdent

-- comment removal for XML files

removeCommentsX [] = []
removeCommentsX ('<':'!':'-':'-':s) = processCommentX s
removeCommentsX (c:s) = c : removeCommentsX s
processCommentX [] = []
processCommentX ('-':'-':'>':s) = removeCommentsX s
processCommentX (c:s) = processCommentX s

-- printing XML documents

prDocX :: DocX -> [String]
prDocX (dtd,tree) =
 prPrologX ++
 [""] ++
 prDTDlocal dtd ++
 [""] ++
 prElementX tree

prPrologX  = ["<?xml version=\"1.0\"?>"] ---

prDTDlocal dtd =
 case dtd of
  ElemX (TagX tag) _ : _ ->
     ("<!DOCTYPE" +++ tag) :
     "["                   :
     map (indent 1) (prDTD dtd)  ++
     ["]>"] 
  _ ->
     ["EMPTY OR INVALID DTD"]

prDTD :: DTD -> [String]
prDTD = map prRuleX

prRuleX :: RuleX -> String
prRuleX r =
 case r of
   ElemX  (TagX tag) typ    -> "<!ELEMENT" +++ tag +++ prTypeX typ +++ ">"
   EntitX ident def         -> "<!ENTITY"  +++ ident +++ "'" ++ def ++ "'" +++ ">"
   AttlistX (TagX tag) list -> "<!ATTLIST" +++ tag +++ 
                               prTList " " (map prAttr list) +++ ">"
                                 where prAttr attr = attr +++ "CDATA #REQUIRED"

prTypeX :: TypeX -> String
prTypeX t =
 case t of
   EmptyX       -> "EMPTY"
   ArgSeqX args -> "(" ++ prArgX 1 args ++ ")"

prArgX :: Int -> ArgX -> String
prArgX n a =
 case (n,a) of
   (_,PCDataX)        -> "#PCDATA"
   (_,AtomX (TagX t)) -> t
   (_,ProdX n (TagX t)) -> prReplicate n "AbsX, " ++ t
   (_,StarX arg)      -> "(" ++ prArgX 1 arg ++ ")" ++ "*"
   (_,PlusX arg)      -> "(" ++ prArgX 1 arg ++ ")" ++ "+"
   (_,OptX  arg)      -> "(" ++ prArgX 1 arg ++ ")" ++ "?"
   (_,SeqX arg1 arg2) -> parAlt arg1 ++  "," +++ parAlt arg2
   (_,AltX arg1 arg2) -> parSeq arg1 +++ "|" +++ parSeq arg2
  where
   parSeq arg = case arg of SeqX _ _ -> "(" ++ prArgX 2 arg ++ ")"
                            _        ->        prArgX 2 arg
   parAlt arg = case arg of AltX _ _ -> "(" ++ prArgX 2 arg ++ ")"
                            _        ->        prArgX 2 arg

prElementX :: ElemX -> [String]
prElementX = prElemX 0

prElemX :: Int -> ElemX -> [String] -- Int gives indentation
prElemX n elemx =
 case elemx of
   VarX (TagX tag, ident) ->  
     (replicate n ' ' ++ "<"  ++ tag ++ ">") :
     (replicate (n + 1) ' ' ++ "<VarX symb = \""  ++ ident ++ "\" />") :
     [replicate n ' ' ++ "</" ++ tag ++ ">"]
   MetaX (TagX tag, ident) ->  
     (replicate n ' ' ++ "<"  ++ tag ++ ">") :
     (replicate (n + 1) ' ' ++ "<MetaX symb = \""  ++ ident ++ "\" />") :
     [replicate n ' ' ++ "</" ++ tag ++ ">"]
   AbsX idents tree -> 
     map (\i -> replicate n ' ' ++ "<AbsX symb = \""  ++ i ++ "\" />") idents ++
     prElemX n tree
   AppX (TagX tag) [] -> [replicate n ' ' ++ "<"  ++ tag +++ "/>"]
   AppX (TagX tag) trees ->  
     (replicate n ' ' ++ "<"  ++ tag ++ ">") :
     foldl (++) [] (map (prElemX (n+1)) trees) ++
     [replicate n ' ' ++ "</" ++ tag ++ ">"]
   PredefX (ident,string) ->  
     (replicate n ' ' ++ "<"  ++ ident ++ ">") :
     map ((replicate (n+1) ' ') ++) (lines string) ++
     [replicate n ' ' ++ "</" ++ ident ++ ">"]

-- auxiliary

firstTagOfDTD :: DTD -> TagX
firstTagOfDTD dtd =
 case dtd of ElemX t _ : _ -> t
             _             -> TagX "??"

-- test

ioDTDfile =
 do
 f <- getLine
 s <- readFile f
 putStr (show (parses pDTD s))

ioDocXfile =
 do
 f <- getLine
 s <- readFile f
 putStr (show (parses pDocX s))


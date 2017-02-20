module AlfLex(lexanal,TOKEN(..),Pos(..),eot,eotPos,PosTOKEN(..),string_of_token,is_ph,num_of_string,ismyAlphanum) where
-- a simpl lexical analyser
import Char(isAlpha,isDigit,ord) -- don't use isAlpha[Nn]um!
import HO(apFst)

--ord = fromEnum :: (Char->Int)

data TOKEN
  = Iden String		-- identifier
  | Con String		-- constructors
  | Symbol String	-- keywords and delimiters
  | Comment' String	-- comments
  deriving (Show,Eq)

iden s@('$':_) = Con s
iden s@('#':_) = Con s
iden s = Iden s

type Pos = (Int,Int)
type PosTOKEN = (Pos,TOKEN)

type PosString = [(Pos,Char)]

string_of_token :: TOKEN -> String
string_of_token (Iden s) = s
string_of_token (Con s) = s
string_of_token (Symbol s) = s

sym :: String -> TOKEN
sym = Symbol

eotPos = (999999,999999)::Pos -- ehum...
eot = (eotPos,sym "EOT")

layout :: Char -> Bool
layout x = elem x "\n \t@!"

keyword :: String -> Bool
keyword x = 
 elem x ["case","let","Set",
         "Theory","Type","data","sig","struct","theory","in","of",
	 "decl","#include"]

ismyAlphanum :: Char -> Bool
ismyAlphanum x = isAlpha x || isDigit x || x == '_'
                 -- || x == '$' || x == '#' || x=='\''

symbolchar,infixchar :: Char -> Bool
symbolchar x = elem x ":;,{}[]()=<>.\\"
infixchar  x = elem x "!%&*+-/?@^|~=<>."

keysymbol x = elem x ["=","<",">","."]

symbolcheck s = if keysymbol s then Symbol s else Iden s

keycheck :: String -> TOKEN
keycheck s = if keyword s then Symbol s else iden s

lexanal = tokenize . addPositions

addPositions s = [((row,column),c) | (row,line) <- zip [1..] (lines s),
                                     (column,c) <- zip [0..] ('\n':line)]
				     -- minor bug: prepends '\n'
				     -- (simple fix for the fact that lines
				     -- removes the '\n' between lines.)

tokenize :: PosString -> [PosTOKEN]
tokenize [] = [eot]
tokenize ((p,'-'):(_,'>'):rest) = (p,sym"->"):(tokenize rest)
tokenize ((p,'['):rest) = (p,Iden"?"):(tokenize (skipgoal rest))
tokenize ((p,'{'):(_,'-'):rest1) = (p,Comment' s):tokenize rest2
   where (s,rest2) =getcomment rest1
tokenize ((p,'"'):rest) = getstring p [] rest
tokenize ((p,a):rest) | layout a = tokenize rest
		      | infixchar a = getinfix p [a] rest
                      | symbolchar a = (p,sym [a]):(tokenize rest)
		      | otherwise = getword p [a] rest

skipgoal :: PosString -> PosString
skipgoal [] = []
skipgoal ((_,']'):rest) = rest
skipgoal (_:rest) = skipgoal rest

getcomment :: PosString -> (String,PosString)
getcomment [] = ("",[])
getcomment ((_,'-'):(_,'}'):rest) = ("",rest)
getcomment ((_,c):rest) = apFst (c:) (getcomment rest)

getword p s [] = [(p,keycheck s),eot]
getword p s (l@((_,x):rest)) | layout       x = (p,keycheck s):tokenize rest
                             | ismyAlphanum x = getword p (s++[x]) rest
			     | otherwise      = (p,keycheck s):tokenize l

getinfix p s [] = [(p,symbolcheck s),eot]
getinfix p s (l@((_,x):rest)) | layout    x = (p,symbolcheck s):tokenize rest
                              | infixchar x = getinfix p (s++[x]) rest
			      | otherwise   = (p,symbolcheck s):tokenize l

getstring p s [] = [(p,iden s),eot]
getstring p s ((_,'\\'):(_,c):rest) = getstring p (s++['\\',c]) rest
getstring p s ((_,'"'):rest) = (p,iden s):(tokenize rest)
getstring p s ((_,x):rest) = getstring p (s++[x]) rest


is_ph :: String -> Bool
is_ph ('?':_) = True
is_ph _ = False

num_of_string :: String -> Int
num_of_string u =
 ns (reverse u)
  where ns [] = 0
        ns (x:xs) = ord x - ord '0' + 10 * (ns xs)


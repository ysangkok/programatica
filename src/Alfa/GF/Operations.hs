module Operations where

import Char (isSpace, toUpper, isSpace)
import List (nub, sortBy, sort)

infixr 5 +++
infixr 5 ++-
infixr 5 ++++
infixr 5 +++++
infixl 9 !!!
infixl 9 !?

-- some auxiliary GF operations. AR 19/6/1998 -- 6/2/2001
-- Copyright (c) Aarne Ranta 1998-2000, under GNU General Public License (see GPL)

-- a safer version of !!
(!!!) :: [a] -> Int -> a
xs !!! i = case xs of
  [x] -> x
  x:xx -> if i == 0 then x else xx !!! (i-1)
-- let i' = min (length xs - 1) i in xs !! i'  -- bad for infinite lists

-- another one with the error monad
(!?) :: [a] -> Int -> Err a
xs !? i = if length xs < i+1 then Bad "too few items in list " else return $ xs !! i

-- useful Error type

data Err a = Ok a | Bad String   -- like Maybe type with error msgs
 deriving (Read, Show, Eq)

instance Monad Err where
 return      = Ok
 Ok a  >>= f = f a
 Bad s >>= f = Bad s

maybeErr :: String -> Maybe a -> Err a
maybeErr s (Just c)  = Ok c         -- add msg s to Maybe failures
maybeErr s (Nothing) = Bad s

okError :: Err a -> a
okError c = case c of 
              Ok a -> a
              _    -> error "no result Ok"

isNotError :: Err a -> Bool
isNotError c = case c of 
                 Ok a -> True
                 _    -> False

testErr :: Bool -> String -> Err ()
testErr cond msg = if cond then return () else Bad msg

showBad :: Show a => String -> a -> Err b
showBad s a = Bad (s +++ show a)

lookupErr :: (Eq a,Show a) => a -> [(a,b)] -> Err b
lookupErr a abs = maybeErr ("Unknown" +++ show a) (lookup a abs)

lookupErrMsg :: (Eq a,Show a) => String -> a -> [(a,b)] -> Err b
lookupErrMsg m a abs = maybeErr (m +++ "gave unknown" +++ show a) (lookup a abs)


lookupDefault :: Eq a => b -> a -> [(a,b)] -> b
lookupDefault d x l = case lookup x l of 
  Just c -> c
  _      -> d

updateLookupList ::  Eq a => (a,b) -> [(a,b)] -> [(a,b)]
updateLookupList ab abs = insert ab [] abs where
 insert c cc [] = cc ++ [c]
 insert (a,b) cc ((a',b'):cc') = if   a == a' 
                                 then cc ++ [(a,b)] ++ cc'
                                 else insert (a,b) (cc ++ [(a',b')]) cc'

mapPairListM :: Monad m => ((a,b) -> m c) -> [(a,b)] -> m [(a,c)]
mapPairListM f xys =
  do yy' <- mapM f xys
     return (zip (map fst xys) yy')

mapPairsM :: Monad m => (b -> m c) -> [(a,b)] -> m [(a,c)]
mapPairsM f xys =
  do let (xx,yy) = unzip xys
     yy' <- mapM f yy
     return (zip xx yy')

pairM :: Monad a => (b -> a c) -> (b,b) -> a (c,c)
pairM op (t1,t2) =
          do t1' <- op t1
             t2' <- op t2
             return (t1',t2')

trc :: Show a => Err a -> Err a  --- to trace
trc e = do a <- e
           seq (putStr (show a)) (return a)

-- like mapM, but continue instead of halting with Err
mapErr :: (a -> Err b) -> [a] -> Err ([b], String) 
mapErr f xs = Ok (ys, unlines ss) 
  where
    (ys,ss) = ([y | Ok y <- fxs], [s | Bad s <- fxs])
    fxs = map f xs

errVal :: a -> Err a -> a
errVal a e = case e of
  Ok x -> x
  _ -> a

errList = errVal []

errIn :: String -> Err a -> Err a
errIn msg e = case e of
  Bad s -> Bad (s ++++ "OCCURRED IN" ++++ msg)
  _ -> e

singleton = (:[])

-- checking

checkUnique :: (Show a, Eq a) => [a] -> [String]
checkUnique ss = ["overloaded" +++ show s | s <- nub overloads] where
  overloads = filter overloaded ss
  overloaded s = length (filter (==s) ss) > 1

titleIfNeeded :: a -> [a] -> [a]
titleIfNeeded a [] = []
titleIfNeeded a as = a:as

errMsg :: Err a -> [String]
errMsg (Bad m) = [m]
errMsg _ = []

-- binary search trees

data BinTree a = NT | BT a (BinTree a) (BinTree a) deriving (Show,Read)

lookupTree :: (Ord a) => (a -> String) -> a -> BinTree (a,b) -> Err b
lookupTree pr x tree = case tree of
 NT -> Bad ("no occurrence of element" +++ pr x)
 BT (a,b) left right 
   | x < a  -> lookupTree pr x left
   | x > a  -> lookupTree pr x right
   | x == a -> return b

lookupTreeEq :: (Ord a) => 
     (a -> String) -> (a -> a -> Bool) -> a -> BinTree (a,b) -> Err b
lookupTreeEq pr eq x tree = case tree of
 NT -> Bad ("no occurrence of element equal to" +++ pr x)
 BT (a,b) left right 
   | eq x a -> return b     -- a weaker equality relation than ==
   | x < a  -> lookupTreeEq pr eq x left
   | x > a  -> lookupTreeEq pr eq x right

updateTree :: (Ord a) => (a,b) -> BinTree (a,b) -> BinTree (a,b)
updateTree z@(x,y) tree = case tree of
 NT -> BT z NT NT
 BT c@(a,b) left right 
   | x < a  -> let left' = updateTree z left in   BT c left' right 
   | x > a  -> let right' = updateTree z right in BT c left right' 
   | otherwise -> BT z left right -- removing the old value of a

updateTreeEq :: 
  (Ord a) => (a -> a -> Bool) -> (a,b) -> BinTree (a,b) -> BinTree (a,b)
updateTreeEq eq z@(x,y) tree = case tree of
 NT -> BT z NT NT
 BT c@(a,b) left right
   | eq x a -> BT (a,y) left right -- removing the old value of a 
   | x < a  -> let left' = updateTree z left in   BT c left' right 
   | x > a  -> let right' = updateTree z right in BT c left right' 

updatesTree :: (Ord a) => [(a,b)] -> BinTree (a,b) -> BinTree (a,b)
updatesTree (z:zs) tr = updateTree z t where t = updatesTree zs tr
updatesTree [] tr = tr

buildTree :: (Ord a) => [(a,b)] -> BinTree (a,b)
buildTree = sorted2tree . sortBy fs where
  fs (x,_) (y,_) 
    | x < y = LT
    | x > y = GT
    | True  = EQ 
-- buildTree zz = updatesTree zz NT

sorted2tree :: [(a,b)] -> BinTree (a,b)
sorted2tree [] = NT
sorted2tree xs = BT x (sorted2tree t1) (sorted2tree t2) where
  (t1,(x:t2)) = splitAt (length xs `div` 2) xs

mapTree :: (a -> b) -> BinTree a -> BinTree b
mapTree f NT = NT
mapTree f (BT a left right) = BT (f a) (mapTree f left) (mapTree f right)

mapMTree :: Monad m => (a -> m b) -> BinTree a -> m (BinTree b)
mapMTree f NT = return NT
mapMTree f (BT a left right) = do
 a'     <- f a
 left'  <- mapMTree f left 
 right' <- mapMTree f right
 return $ BT a' left' right' 

tree2list :: BinTree a -> [a] -- inorder
tree2list NT = []
tree2list (BT z left right) = tree2list left ++ [z] ++ tree2list right

depthTree :: BinTree a -> Int
depthTree NT = 0
depthTree (BT _ left right) = 1 + max (depthTree left) (depthTree right)

-- parsing

--type Parser a b = ... -- now exported from ParserType
type WParser a b = [a] -> [(b,[a])] -- old Wadler style parser

wParseResults :: WParser a b -> [a] -> [b]
wParseResults p aa = [b | (b,[]) <- p aa]

-- printing

indent :: Int -> String -> String
indent i s = replicate i ' ' ++ s

a +++ b   = a ++ " "    ++ b
a ++- ""  = a 
a ++- b   = a +++ b
a ++++ b  = a ++ "\n"   ++ b
a +++++ b = a ++ "\n\n" ++ b

prUpper :: String -> String
prUpper s = s1 ++ s2' where
 (s1,s2) = span isSpace s
 s2' = case s2 of
   c:t -> toUpper c : t
   _ -> s2

prReplicate n s = concat (replicate n s)

prTList t ss =
 case ss of
  []   -> ""
  [s]  -> s
  s:ss -> s ++ t ++ prTList t ss

prQuotedString x = "\"" ++ restoreEscapes x ++ "\""

prParenth s = if s == "" then "" else "(" ++ s ++ ")"

prCurly s = "{" ++ s ++ "}"
prBracket s = "[" ++ s ++ "]"

prArgList xx = prParenth (prTList "," xx)

prSemicList = prTList " ; "

prCurlyList = prCurly . prSemicList

restoreEscapes s = 
  case s of 
    []       -> []
    '"' : t  -> '\\' : '"'  : restoreEscapes t
    '\\': t  -> '\\' : '\\' : restoreEscapes t
    c   : t  -> c : restoreEscapes t

numberedParagraphs :: [[String]] -> [String]
numberedParagraphs t =
 case t of 
  []   -> []
  p:[] -> p
  p:k  -> foldl (++) [] [(show n ++ ".") : s | (n,s) <- zip [1..] t]

prConjList :: String -> [String] -> String
prConjList c []     = ""
prConjList c [s]    = s
prConjList c [s,t]  = s +++ c +++ t
prConjList c (s:tt) = s ++ "," +++ prConjList c tt

prIfEmpty :: String -> String -> String -> String -> String
prIfEmpty em _    _    [] = em
prIfEmpty em nem1 nem2 s  = nem1 ++ s ++ nem2

wrapLine n = unlines . cutLine1 n
cutLine1 :: Int -> String -> [String]
cutLine1 n s = 
 collectLines [] s 
  where
   collectLines ss [] = ss 
   collectLines ss s = 
    case splitAt n s of 
      (s1,s2) -> 
           collectLines (ss ++ [s1 ++ s21]) s22
             where 
                (s21,s22) =
                  case span (not . (`elem` " ,\n.")) s2 of 
                         (x,[])  -> (x,[])
                         (x,z:y) -> (x++[z],y) 

cutLine _ = lines . wrapLines 0
-- Thomas Hallgren's wrap lines
--- optWrapLines = if argFlag "wraplines" True then wrapLines 0 else id
wrapLines n "" = ""
wrapLines n s@(c:cs) =
      if isSpace c
      then c:wrapLines (n+1) cs
      else case lex s of
            [(w,rest)] -> if n'>=76
                          then '\n':w++wrapLines l rest
                          else w++wrapLines n' rest
               where n' = n+l
                     l = length w
            _ -> s -- give up!!

-- LaTeX code producing functions

dollar s = '$' : s ++ "$"
mbox s   = "\\mbox{" ++ s ++ "}"
ital s   = "{\\em" +++ s ++ "}"
boldf s  = "{\\bf" +++ s ++ "}"
verbat s = "\\verbat!" ++ s ++ "!"

begindocument =
 "\\documentstyle[isolatin1]{article}" ++++
 "\\setlength{\\parskip}{2mm}" ++++
 "\\setlength{\\parindent}{0mm}" ++++
 "\\setlength{\\oddsidemargin}{0mm}" ++++
 "\\setlength{\\evensidemargin}{-2mm}" ++++
 "\\setlength{\\topmargin}{-8mm}" ++++
 "\\setlength{\\textheight}{240mm}" ++++
 "\\setlength{\\textwidth}{158mm}" ++++
 "\\begin{document}\n"

enddocument =
 "\n\\end{document}\n"

readFileIf :: String -> IO String
readFileIf f = catch (readFile f) (\_ -> reportOn f) where
 reportOn f =
   do
   putStr ("File " ++ f ++ " does not exist. Returned empty string")
   return ""

exclExcept :: [a] -> Int -> a -> a
exclExcept list i exc = if length list <1 then list !! i else exc

sortByLongest :: [[a]] -> [[a]]
sortByLongest = sortBy longer where
 longer x y 
  | x' > y' = LT
  | x' < y' = GT
  | True    = EQ
  where
   x' = length x
   y' = length y

combinations :: [[a]] -> [[a]]
combinations t = case t of 
  []    -> [[]]
  aa:uu -> [a:u | a <- aa, u <- combinations uu]

mkTextFile :: String -> IO ()
mkTextFile name = do
  s <- readFile name
  let s' = prelude name ++ "\n\n" ++ heading name  ++ "\n" ++ object s
  writeFile (name ++ ".hs") s'
 where
   prelude name = "module " ++ name ++ " where" 
   heading name = "txt" ++ name ++ " ="
   object s = mk s ++ " \"\""
   mk s = unlines ["  \"" ++ escs line ++ "\" ++ \"\\n\" ++" | line <- lines s]
   escs s = case s of
     c:cs | elem c "\"\\" -> '\\' : c : escs cs
     c:cs -> c : escs cs
     _ -> s

initFilePath :: FilePath -> FilePath
initFilePath f = reverse (dropWhile (/='/') (reverse f))

-- topological sorting with test of cyclicity

topoTest :: Eq a => [(a,[a])] -> Either [a] [a]
topoTest g = if length g' == length g then Left g' else Right (cycleIn g') where
    g' = topoSort g
    cycleIn = cyc []
    cyc vis k = case k of
      x:xs 
        | elem x vis -> x : reverse (x : takeWhile (/=x) vis)
	| otherwise  -> cyc (x:vis) xs
      [] -> []

topoSort :: Eq a => [(a,[a])] -> [a]
topoSort g = reverse $ tsort 0 [ffs | ffs@(f,_) <- g, inDeg f == 0] [] where
  tsort _ []     r = r
  tsort k (ffs@(f,fs) : cs) r
    | elem f r  = tsort k cs r
    | k > lx    = r  
    | otherwise = tsort (k+1) cs (f : tsort (k+1) (info fs) r)
  info hs = [(f,fs) | (f,fs) <- g, elem f hs]
  inDeg f = length [t | (h,hs) <- g, t <- hs, t == f]
  lx = length g

type FontId = String

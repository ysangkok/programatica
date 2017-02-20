module Profile where

import List (nub)

import Operations
import Grammar
import Macros
import CF
import GFCFIdents

-- restoring parse trees for discontinuous constituents, bindings, etc. AR 25/1/2001

postParse :: GFCFTree -> Err Trm
postParse cftree = do
  let tree = cftree2tree cftree --- transient ! 
  term <- maybeErr "discontinuity failed" $ tree2term tree
  maybeErr "bindings failed"              $ term2trm  term
  -- remains to refresh w.r.t. a grammar

-- an intermediate data structure
data ITerm = ITerm FunBind [ITerm] | Mett GFCFCat deriving (Eq,Show)

type PFun    = Trm
type FunBind = (PFun,Binds)
type FunPro  = (FunBind,Profile)

-- use this provisorily; we'd rather parse into Tree directly
data Tree = Tree FunPro [Tree] deriving (Eq,Show)
cftree2tree :: GFCFTree -> Tree
cftree2tree (CFTree (GFCFFun f, (_,tt))) = 
  let tt' = map cftree2tree tt in Tree f tt'

-- the job is done in two passes: 
-- (1) restore constituent order from Profile 
-- (2) restore Bindings from Binds

tree2term :: Tree -> Maybe ITerm
tree2term (Tree ((Meta (MetaSymb (c,_)),_),_)  _) = return $ Mett (symid c)
tree2term (Tree (fun,pro) trees) = do
  args <- mapM mkArg pro
  return $ ITerm fun args
 where
   mkArg (_,[x]) = tree2term (trees !! x)
   mkArg (c,[])  = return $ Mett (symid c)
   mkArg (c,xs)  = do
     let trees' = map (trees !!) xs
     xs1 <- mapM tree2term trees' 
     xs2 <- checkArity xs1
     unif (symid c) xs2

   checkArity xs = 
     if length (nub [length xx | ITerm _ xx <- xs']) >1 then Nothing else return xs'
       where xs' = [t | t@(ITerm _ _) <- xs]
   unif c [] = return $ Mett c
   unif c xs@(ITerm f xx : _) = do
     f'  <- unifFun
     xx' <- mapM (unifArg c) [0 .. arity]
     return $ ITerm f' xx'
    where
      arity = length xx - 1
      unifFun = if all (==f) [h | ITerm h _ <- xs] then return f else Nothing
      unifArg c i = tryUnif c [zz !! i | ITerm _ zz <- xs]
   tryUnif c xx = case [t | t@(ITerm _ _) <- xx] of
     [] -> return $ Mett c
     x:xs -> if all (==x) xs then return x else Nothing

term2trm :: ITerm -> Maybe Trm
term2trm (Mett c) = return $ Meta (MetaSymb (zIdent c, 0))
term2trm (ITerm (fun,binds) terms) = do
  let terms' = [(t, elem i binds) | (t,i) <- zip terms [0..]]
  tt <- mkTrms terms'  
  return $ foldl App fun tt
 where
   mkTrms ((t,b):tbs) = 
     if b then do
       x <- mkVar t  -- fails if t is not a variable
       mkAbss [x] tbs
     else do 
       t'   <- term2trm t 
       tbs' <- mkTrms tbs
       return $ t' : tbs'
   mkTrms [] = return []
   mkAbss xs ((t,b):tbs) =
     if b then do
       x <- mkVar t  -- fails if t is not a variable
       mkAbss (xs ++ [x]) tbs
     else do 
       t' <- term2trm t 
       tbs' <- mkTrms tbs
       return $ foldr Abs t' xs : tbs'
   mkAbss _ [] = Nothing         -- bindings without body
   mkVar (ITerm (Var x,[]) []) = return x
   mkVar (Mett _) = return (zid "#h")      -- generate a new variable symbol
   mkVar _ = Nothing
   zid = zIdent

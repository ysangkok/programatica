module GFtoCF where

import Operations
import Tokens
import Grammar
import PrGrammar
import CF
import SymbolTable
import ComputeTerm
import Lookup
import Macros
import LinTypes
import Linearize
import Predefined
import GFCFIdents
---- import PPrCF (prCFItem)
import List (nub, partition)

-- AR 27/1/2000 -- 11/4 -- 26/1/2001

-- the main function

grammar2cf :: Token a => GrammarST [a] -> (Concrete [a]) -> Err (GFCF a, String)
grammar2cf = grammar2cfReduced (const True)

grammar2cfReduced :: 
  Token a => (Fun -> Bool) -> GrammarST [a] -> (Concrete [a]) -> Err (GFCF a,String)
grammar2cfReduced cond gr (Concrete ldefs) =             --- do these 3 on 1 pass !
  do (rrr,ss) <- mapErr (rule2cf gr) [f | DefLin f _ _ <- ldefs, cond f]
     let cats = [cat      | cat <- allCats (fst gr)]  
         vars = [(x, cat) | DefVar x cats    <- ldefs, cat <- cats]
     return ((concat (map fst rrr), mkPredefCF cats vars),
             "CF rules for" +++ unwords (map snd rrr) ++++ ss)

tryUpdateCF :: Token a => GrammarST [a] -> GFCF a -> Fun -> GFCF a
tryUpdateCF gr cf@(rr0,lits) fun = case rule2cf gr fun of
  Ok (rr,m) -> (rr ++ rr0, lits) --- m is an error message
  _ -> cf

reduceCF :: Token a => [Fun] -> GFCF a -> GFCF a
reduceCF funs (rules,pre) = (filter (not . away) rules, pre) where
  away rule = any (flip isRuleForFun rule) funs

-----

rule2cf :: Token a => GrammarST [a] -> Fun -> Err ([GFCFRule a], String)
rule2cf gr@(abs,conc) fun = do
     typ  <- lookupFun fun abs
     typf0@(args0,val0) <- typeSkeleton typ

     -- the important auxiliaries in the algorithm
     let typf@(args,val) = ([(x,mkGFCFCat c) | (x,c) <- args0], mkGFCFCat val0)
         flatArgs = concat [replicate x catVarGFCF ++ [c] | (x,c) <- args]
         lnb i = fst $ args !! i
         cat i = snd $ args !! i
         pos i = sum [lnb k + 1 | k <- [0..i]] - 1
         bindP = concat [[pos i - l + k | k <- [0..l-1]] | 
                                      i <- [0..length args - 1], let l = lnb i]
         argVs = [ (cat, (pos i, xx)) | ((xx,cat),i) <- zip args0 [0..]]

     lterm   <- errIn ("linWithVars"  +++ prt fun) $ linWithVars argVs
     parts   <- findLinLabels lterm

     (tt,ss) <- mapErr (lterm2cf (val,bindP,flatArgs)) parts
     let tt' = nub (concat tt)
         ss' = prt fun +++ show (length tt')
     ttss2 <- testCircular (tt', ss' +++ ss)
     return ttss2

  where

      -- linWithVars :: [(Cat,(Int,Int))] -> Err (LTerm [a])
      linWithVars argvs = do
         args'  <- errIn ("make CF args" +++ prt fun) $ return $ map mkCFArg argvs
         (xx,t) <- lookupLinearization fun conc
         lin  <- errIn ("linCompute" +++ prt fun) (substitute (zip xx args') t)
         betaconv lin

      mkCFArg (cat,(pos,bi)) = ArgVar (cat,(pos,bi))

      -- lterm2cf :: (Int,Term [a]) -> Err [GFCFRule a]
      lterm2cf (val,bindP,flat) (disc, tr) = do
        let cat = mkGFCFCatDisc disc val
        itss  <- term2CFItems tr
        profs <- return $ map (mkProfile flat) itss
        return [(GFCFFun ((Cons fun, bindP), prof), (cat, map precf2cf its)) | 
                                       (its,prof) <- zip itss profs]

      -- mkProfile :: [PreGFCFItem a] -> Err Profile
      mkProfile flat items = [(zIdent c, occs i) | (c,i) <- zip flat [0..]]
        where
          occs i = [k | (n,k) <- zip [n | PNonterm _ n <- items] [0..], n == i]

      testCircular (tt,ss) = do
        let (tt1,tt2) = partition isCircularCF tt
        case tt1 of
          [] -> return (tt,ss)
          _  -> return (tt2,ss +++ "(-" +++ show (length tt1) +++ "circular)")

-----

term2CFItems :: Token a => LTerm [a] -> Err [[PreGFCFItem a]]
term2CFItems t = case t of
   Select c _ -> do
     t2c c
   Cases cc -> do
     its  <- mapM (t2c . snd) cc
     its' <- tryMkCFTerm (concat its)
     return its'
   Concat t1 t2 -> do
     its1 <- t2c t1
     its2 <- t2c t2
     return [x ++ y | x <- its1, y <- its2]
   Glue t1 t2 -> do
     its1 <- t2c t1
     its2 <- t2c t2
     mapM glueCFItems [(x,y) | x <- its1, y <- its2]
   Alts _ (t,tt) -> do
     its1 <- t2c t
     its2 <- mapM (t2c . snd) tt
     return (its1 ++ concat its2)

   Tok aa -> return [map (PTerm . RegAlts . (:[])) aa]

---   Strs [t] -> t2c t
---   Cons f -> do
---     t <- lookupLin f gr
---     t2c t
---   App c a -> do
---     (_,c',_) <- termForm t
---     t2c c'
---   ArgVar (cat,(pos,bi)) -> 
---     return [[CFNonterm cat]]
---
---   ArgVar (cat,(pos,_)) ->                        ----- strange case ! 
---     return [[PNonterm (mkGFCFCatDisc 0 (mkGFCFCat cat)) pos]]

   Project (ArgVar (cat,(pos,_))) s@(Label (_,k)) | linLabel k == s -> 
     return [[PNonterm (mkGFCFCatDisc k (mkGFCFCat cat)) pos]]
   Project (ArgVar (_,(pos,bi)))  v@(Label (_,k)) | varLabel k == v -> 
     return [[PNonterm catVarGFCF (pos + k - bi)]]

   _ -> prtBad "cannot form cf items from" t
  where 
    t2c = term2CFItems

tryMkCFTerm :: Token a => [[PreGFCFItem a]] -> Err [[PreGFCFItem a]]  
tryMkCFTerm ii@(its:itss) | all (\x -> length x == length its) itss =
  case mapM mkOne (counterparts ii) of
    Ok tt -> return [tt]
    _ -> return ii
   where
    mkOne cfits = case mapM mkOneTerm cfits of
      Ok tt -> return $ PTerm (RegAlts (concat (nub tt)))
      _ -> mkOneNonTerm cfits
    mkOneTerm (PTerm (RegAlts t)) = return t
    mkOneTerm _ = Bad ""
    mkOneNonTerm (PNonterm c n : cc) = 
      if all (==PNonterm c n) cc then return (PNonterm c n) else Bad ""
    mkOneNonTerm _ = Bad ""
    counterparts ll = [map (!! i) ll | i <- [0..length (head ll) - 1]]
tryMkCFTerm itss = return itss

glueCFItems :: Token a => 
               ([PreGFCFItem a],[PreGFCFItem a]) -> Err [PreGFCFItem a]
glueCFItems ([],y) = return y
glueCFItems (x,[]) = return x
glueCFItems (x,PTerm (RegAlts bs) : y) = case last x of
  PTerm (RegAlts as) -> 
    return 
      (init x ++ 
       map (PTerm . RegAlts) (combinations [glueTok [a] [b] | a <- as, b <- bs]) 
            ++ y)  
  _ -> Bad "no matching cfterm to prefix"  --- better msg
{-
glueCFItems (x,CFTerm (RegAlts [b]) : y) = case last x of
  CFTerm (RegAlts [a]) -> 
    return (init x ++ map mkCFTerm (glueTok [a] [b]) ++ y)
  _ -> Bad "no matching cfterm to prefix"  --- better msg
-}
glueCFItems (x,y) = 
  Bad "cannot glue cf items" 
  -- +++ concatMap prCFItem x +++ concatMap prCFItem  y)


-- functions creating parsers for variables and metavariables
mkPredefCF cats vars t = literalCFPredef t ++ metaCF t ++ boundCF t ++ varCF t
  where
    metaCF t  = [(mkGFCFCat c, mkGFFun (Meta m)) | 
                      c <- cats, let m = mkFreshMeta [] c, 
                      showTok t`elem` [prt m, prt c]] --- both [?C?] and C
    boundCF t = [(catVarGFCF,  mkGFFun (Var x))  | (x,_) <- vars, isVar x t]
    varCF t   = [(mkGFCFCat c, mkGFFun (Var x))  | (x,c) <- vars, isVar x t]

    isVar x t = let (s,ps) = span (/='\'') (showTok t) in 
                               s == prt x && all (=='\'') ps


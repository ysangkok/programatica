module LocalUndo where

import Operations
import Grammar
import Macros
import SymbolTable
import Lookup
import Update (mkTyped)
import PrGrammar
import Refine
import TC ()
import BottomUp

removeSubTerm :: AbstractST -> TermInfo -> Int -> Maybe (Fun,[Int]) -> Err TermInfo
removeSubTerm gr ti i mbfi = do
  tree0 <- mkTermTree gr trm
  let (tree1,tree2) = splitAt i tree0
      tree3         = dropWhile ((>dth) . fst) (tail tree2)
      (dth,(xx,strm)) = tree0 !! i 
  term2 <- case mbfi of   -- Nothing = delete

             Just (f,ii) -> do
	            typ       <- lookupFun f gr  ---  
  	            (args,_,_) <- typeForm typ ---
                    wt        <- mkWrappedTerm oldmetas strm ((f,ii),args)
                    (zz,h,yy) <- termForm wt
                    return (dth,(xx ++ zz, mkApp h yy)) 

             _ -> return (dth,(xx, Meta (meSy strm)))
  let tree = if length tree0 <= i then tree0 else tree1 ++ [term2] ++ tree3
  let trm' = restoreTermFromTree tree
  let qs' = [] --- [q | q@(m,_) <- qs, elem m [t | (_,(_,Meta t)) <- tree]]
               --- not quite correct as remaining qs since old type remains for q
  let cs' = [] --- constraints concerning qs' only
  checkTermInfo gr (mkTermInfo trm' typ co qs' cs') -- brings in the new meta
 where
   (trm,typ,co,qs,cs) = infoOfTI ti
   oldmetas = map fst qs
   meSy a = case a of
     Cons f -> case lookupFun f gr of Ok ty -> mkValMeta ty
     Typed _ ty -> mkValMeta ty
     Meta m -> m             
     Literal c _ -> mkFreshMeta oldmetas c
     _      -> mkFreshMeta [] (zIdent "??") -- should not happen
   mkValMeta ty = case valCat ty of
     Ok c -> mkFreshMeta oldmetas c

mkTermTree :: AbstractST -> Trm -> Err [(Int,([Ident],Trm))]
mkTermTree abs term = do
  tf <- termForm (mkTyped abs term)
  mktt 0 tf
 where
  mktt n (xx,f,aa) = do
    aa'  <- mapM termForm aa
    aa'' <- mapM (mktt (n+1)) aa'
    return $ (n,(xx,f)) : concat aa''

restoreTermFromTree :: [(Int,([Ident],Trm))] -> Trm
restoreTermFromTree ((n0, (xx,f)):rest) = mkTerm (xx,f,aa) where
 aa = map restoreTermFromTree (findSegs n0 rest)
 findSegs n trs = case trs of
   []   -> []
   r:rr -> (r:rr1) : findSegs n rr2 where (rr1,rr2) = span ((>(n+1)) . fst) rr


module Randomized where

import Operations
import Grammar
import SymbolTable
import Update ---
import Macros
import Lookup
import Refine
import Random

-- 9/5/2000

-- build a lazy list of random terms; use mx to prevent infinite search
mkRandomTerms :: StdGen -> Int -> AbstractST -> Cat -> [Trm]
mkRandomTerms gen0 mx st cat = tryR gen0 mx where
  tryR _ 0 = [] --- mx is used as max number of tries 
  tryR gen0 n = errVal [] $ do
    ti0 <- initTermInfo st cat
    tir <- mkRandomTInfo gen0 mx st ti0
    case tir of
      (ti, gen)
          | isCompleteTI ti -> return $ termOfTI ti : mkRandomTerms gen mx st cat
      (_,gen) -> return $ tryR gen (n - 1)

-- try to build a random term info
mkRandomTInfo :: StdGen -> Int -> AbstractST -> TermInfo -> Err (TermInfo,StdGen)
mkRandomTInfo gen0 mx st ti = mkR gen0 mx ti where
  mkR gen 0 ti = return (ti,gen)
  mkR gen n ti | isCompleteTI ti = return (ti,gen)
  mkR gen n ti = do
    meta <- firstMetaOfTI ti
    let refs = refinementsOfGoal st ti meta
    let cmx  = length refs
    if cmx == 0 then return (ti,gen) 
      else do
        let (int,gen') = randomR (0, max 0 (cmx-1)) gen
        let (ref,typ) = (refs !! int)
        ti' <- refineWithRef st ti meta ref typ
        mkR gen' (n-1) ti'

-- library has mkStdGen :: Int -> StdGen

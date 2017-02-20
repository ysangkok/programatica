module CFProperties where

import Operations
import CF
import PPrCF

showCFProperties cf n = do
  putStrLn $ showCFLength cf
  putStrLn $ showEpsilons cf
  let lfrs = take maxLeftRecs $ findLeftRecN n cf
  putStrLn $ showLeftRecN $ lfrs
  putStrLn $ unlines $ map showCircularN $ lfrs

showCFLength cf@(rr,_) = "The grammar has" +++ show (length rr) +++ "cf rules"
showEpsilons cf@(rr,_) = case epsilons of
  [] -> "No empty productions."
  _  -> "Empty productions" ++++ unlines (map prCFRule epsilons)
 where
  epsilons = [r | r@(_,(_,[])) <- rr]

showLeftRecN rrs = case rrs of
  []  -> "No left recursion detected."
  rrs -> "Left recursion of levels" ++++ unlines rrs'
           where rrs' = (map (\ (n,r) -> show n ++++ unlines (map prCFRule r)) rrs)

showCircularN (n,rs0) = case filter isCircularCF rs0 of
  [] -> []
  rs -> "WARNING: CIRCULAR RULES, in chain of length" +++ show n ++++ unlines rs'
           where 
             rs' = map prCFRule rs

topoTestCF (cf,_) = topoTest [(c,[c']) | (_,(c,CFNonterm c':_)) <- cf]

maxLeftRecs :: Int
maxLeftRecs = 500 ---

findLeftRecN n cf@(rules,_) = tryLeft 0 [] rules where 
 tryLeft k ff rr = case findLeftRec ff rr of
   [] | k < n -> tryLeft (k+1) ff (expandN 1 rr) 
   [] -> []
   cfs | k < n -> [(k,cfs')] ++ tryLeft (k+1) ff' (expandN 1 rr)
                     where
                       cfs' = take maxLeftRecs cfs 
                       ff' = map fst cfs' ++ ff
   cfs -> [(k,take maxLeftRecs cfs)]

findLeftRec ff rr = 
 [r | r@(f,(cat,CFNonterm cat':_)) <- rr, compatCF cat cat', notElem f ff]

expandN 0 rules = rules
expandN n rules =  concat (map (expandFirst rules) rules') where
 rules' = expandN (n-1) rules

productions cat rr = [its | (_,(_,its)) <- rulesFor (rr, emptyCFPredef) cat]

expandFirst rr rule@(fun,(cat, CFNonterm cat' : its)) = 
 [(fun,(cat, its0 ++ its)) | its0 <- productions cat' rr]
expandFirst _ rule = [rule]


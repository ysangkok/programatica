module AgdaLayout where

import LexAgda
--import Alex

-- Generated by the BNF Converter, modified by MB

stripJust :: Maybe a -> a
stripJust (Just x) = x

-- local parameters

topLayout = True
layoutWords = ["of","let","where","sig","struct"]
layoutStopWords = ["in"]

{- Here comes the modification for Agda:

"in" may be a stop-layout token ("let ... in ... )
but it also may be layout-neutral ("open...use...in...")
The layout reolution function now maintains a stack of tokens. 
when a layoutWord or nolayoutWord is encountered it is pushed on the stack
when a layoutStopWord is encountered, one token is popped off the stack, and

   - if it is a layoutWord: exitBlock
   - if it is a nolayoutWord: nothing happens

-}

nolayoutWords = ["use"]

-- by setting the first argument False, you can switch off top-level layout

resolveLayout :: Bool -> [Token] -> [Token] 
resolveLayout tp = tailif . res [] iftop where

  -- the parameters are used in this way
  iftop = if (tp && topLayout) then [1] else []   -- stack of block positions
  tailif = if (tp && topLayout) then tail else id -- remove first ; (hack)
  ifmix t0 = prToken t0 /= openBrace
  isLayout = isTokenIn layoutWords
  isStop = isTokenIn layoutStopWords
  isNolayout = isTokenIn nolayoutWords
   -- res :: [Token] -> [Int] -> [Token] -> [Token]
  res stack st@(n:ns) (t:ts@(t0:_)) 
    | isLayout t && ifmix t0 = enterBlock (t:stack) t st ts  -- test if layout is used
    | column t == n  = sameBlock stack  t st ts
    | column t < n   = exitBlock stack t ns ts -- pop the stack
    | isStop t
    , (top:pop) <- stack
    , isLayout top    = exitBlock pop t ns ts
    | isStop t
    , (top:pop) <- stack
    , isNolayout top = t : res pop st ts
    | isNolayout t   = t : res (t:stack) st ts
    | otherwise      = t : res stack st ts

  -- special introduction of braces if the end of file is encountered
  res last (_:ns) (t:[])  = let Pn g l c = position t in
                        t : [sToken p s | 
                             (p,s) <- zip [Pn (g + i) l (c + i + 1) | i <- [0,2..]]
                                           (replicate (length ns) closeBrace ++ 
                                           [semicolon])]

  enterBlock last t0 st@(n:ns) ts@(t:ts1)
    | ct > n    = t0 : addToken (nextPos t0) openBrace (t : res last (ct:st) ts1)  
    | otherwise = error $ "block enter error at " ++ show (position t)
   where 
     ct = column t 

  sameBlock last t0 st@(n:ns) ts
    | otherwise = addToken (position t0) semicolon (t0 : res last st ts)

  exitBlock stack t0 st@(n:ns) ts
    | column t0 < n = addToken (position t0) closeBrace (exitBlock stack t0 ns ts)
                                                         -- exit to yet outer block
    | otherwise     = addToken (position t0) closeBrace (
                        if isStop t0 
                          then t0 : res stack st ts
                          else res stack st (t0:ts))


addToken :: Position -> String -> [Token] -> [Token]
addToken p s ts = sToken p s : map (incrGlobal p (length s)) ts

type Position = Posn

nextPos :: Token -> Position 
nextPos t = Pn (g + s) l (c + s + 1) where
  Pn g l c = position t
  s = tokenLength t

incrGlobal :: Position -> Int -> Token -> Token
incrGlobal (Pn g0 l0 c0) i (PT (Pn g l c) t) =
  if l > l0 
    then PT (Pn (g + i) l c) t
    else PT (Pn (g + i) l (c + i)) t
incrGlobal (Pn g0 l0 c0) i p = error $ "cannot add token at " ++ show p

sToken :: Position -> String -> Token
sToken p s = PT p (TS s) -- reserved word or symbol

position :: Token -> Position
position t = case t of
  PT p _ -> p
  Err p -> p

line :: Token -> Int
line t = case position t of Pn _ l _ -> l

column :: Token -> Int
column t = case position t of Pn _ _ c -> c

isTokenIn :: [String] -> Token -> Bool
isTokenIn ts t = case t of
  PT _ (TS r) | elem r ts -> True
  _ -> False

tokenLength :: Token -> Int
tokenLength t = length $ prToken t

openBrace  = "{"
closeBrace = "}"
semicolon  = ";"

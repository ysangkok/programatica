module BddTable 
  (
 --   BddTI
  --, BddTableIndex
 -- , initBddOne
 -- , buildBddTI
 -- , applyOpBddTI
 -- ,
   --                         restrictBddTI,  insertBddTable, 
   isBddOne) where

    import BoolAlgebra (BoolExpr(..), BoolOp(..), BoolVar, BoolVal, rewriteBoolExpr,
                            totalEvalBoolExpr, getBoolVal, applyBoolOp, toBv, bvOne, bvZero)
   -- import Utils (fromJust, isJust, isNothing)
--    import QuickCheck
  --  import Monad(liftM,liftM2)
--    import Random
    import Maybe (fromJust, isJust, isNothing)
    import GenNat
    import Bdt (taut,faus)

    type BddTableIndex = Int

    type BddTableEntry = (BddTableIndex, BddTableIndex, BddTableIndex)
                            -- (Variable Index, Low branch pointer, High branch pointer)

    type BddTable = [(BddTableIndex, Maybe BddTableEntry)]

    type BddTI = (BddTable, BddTableIndex)

    -- End of Type Declarations


    initBddTable :: BddTable
    initBddTable = (1, Nothing) : (0, Nothing) : []

    initBddZero :: BddTI
    initBddZero = (initBddTable, 0)

    initBddOne :: BddTI
    initBddOne = (initBddTable, 1)

    toIndex :: BoolVal -> BddTableIndex
    toIndex v = if v then 1 else 0


    -- Primitive Functions

    makeBddTableNode :: BddTable -> BddTableEntry -> BddTI
    makeBddTableNode h (i, v0, v1)
            | (v0 == v1)    = (h, v0)
            | (isJust f)    = (h, fromJust f)
            | otherwise     = (insertBddTable h (i, v0, v1)) where
                                    f = findBddTableEntry h (i, v0, v1)

    insertBddTable :: BddTable -> BddTableEntry -> BddTI
    insertBddTable [] _ = error "BddTable.insertBddTable: table not initialised"
    insertBddTable hs e = ((ni, Just e):hs, ni) where
                            ni = getNextBddTableNode hs

    getNextBddTableNode :: BddTable -> BddTableIndex
    getNextBddTableNode [] = error "BddTable.getNextBddTableNode: table not initialised"
    getNextBddTableNode ((i,_):_) = (i + 1)

    findBddTableEntry :: BddTable -> BddTableEntry -> Maybe BddTableIndex
    findBddTableEntry h e
            | null h2       = Nothing
            | otherwise     = Just (fst $ head h2) where
                                    h2 = dropWhile (f e) h
                                    f :: BddTableEntry -> (BddTableIndex, Maybe BddTableEntry) -> Bool
                                    f _ (_, Nothing) = True
                                    f e1 (_, Just e2) = (e1 /= e2)

    getBddTableEntry :: BddTable -> BddTableIndex -> Maybe BddTableEntry
    getBddTableEntry hs n = snd $ head $ getBddTable hs n

    getBddTable :: BddTable -> BddTableIndex -> BddTable
    getBddTable h i = drop ((getNextBddTableNode h) - i - 1) h

    getBddTI :: BddTI -> BddTableIndex -> BddTI
    getBddTI _ 0 = initBddZero
    getBddTI _ 1 = initBddOne
    getBddTI (h, _) i = (getBddTable h i, i)


    -- Main BddTI Functions
    isBddOne ::BoolExpr -> Bool
    isBddOne t = snd (buildBddTI t vs) == 1
                  where
                     vs = f (Succ m) Zero
                   
                     f Zero k = []
                     f (Succ n) k = k:f n (Succ k)
                     m = i2n(maxVI t)

    buildBddTI :: BoolExpr -> [BoolVar] -> BddTI
    buildBddTI xs vs = buildBddTable xs vs initBddOne --where
    buildBddTable :: BoolExpr -> [BoolVar] -> BddTI -> BddTI
    buildBddTable t [] (h, _) = (h, toIndex $ totalEvalBoolExpr t)
    buildBddTable t (x:xs) (h, i) = makeBddTableNode h1 (i, v0, v1) where
                    (h0, v0) = buildBddTable (rewriteBoolExpr t (bvZero, x)) xs (h, i + 1)
                    (h1, v1) = buildBddTable (rewriteBoolExpr t (bvOne, x)) xs (h0, i + 1)

                               
    applyOpBddTI :: BoolOp -> BddTI -> BddTI -> BddTI -> BddTI
    applyOpBddTI _ ([], _) _ _ = error "BddTable.applyOpBddTI: first BddTable empty"
    applyOpBddTI _ _ ([], _) _ = error "BddTable.applyOpBddTI: second BddTable empty"
    applyOpBddTI op g1 g2 (h, i)
            | (elem u1 [0, 1]) && (elem u2 [0, 1])  = r1 
            | (elem u1 [0, 1]) && (u2 > 1)          = r2
            | (u1 > 1) && (elem u2 [0, 1])          = r3
            | varU1 < varU2                         = r3
            | varU1 > varU2                         = r2
            | otherwise                             = r4 where
                    (((_, e1):_), u1) = g1
                    (((_, e2):_), u2) = g2
                    (varU1, lowU1, highU1) = if isNothing e1 then error "BddTable.applyOpBddTI: table 1 empty"
                                                    else fromJust e1
                    (varU2, lowU2, highU2) = if isNothing e2 then error "BddTable.applyOpBddTI: table 2 empty"
                                                    else fromJust e2
                    gLow1 = getBddTI g1 lowU1
                    gHigh1 = getBddTI g1 highU1
                    gLow2 = getBddTI g2 lowU2
                    gHigh2 = getBddTI g2 highU2
                    r1 = (h, toIndex $ applyBoolOp op (toBv u1) (toBv u2))
                    r2 = makeBddTableNode gr2 (varU2, lv2, hv2) where
                            (ht2, lv2) = applyOpBddTI op g1 gLow2 (h, i + 1)
                            (gr2, hv2) = applyOpBddTI op g1 gHigh2 (ht2, i + 1)
                    r3 = makeBddTableNode gr3 (varU1, lv3, hv3) where
                            (ht3, lv3) = applyOpBddTI op gLow1 g2 (h, i + 1)
                            (gr3, hv3) = applyOpBddTI op gHigh1 g2 (ht3, i + 1)
                    r4 = makeBddTableNode gr4 (varU1, lv4, hv4) where
                            (ht4, lv4) = applyOpBddTI op gLow1 gLow2 (h, i + 1)
                            (gr4, hv4) = applyOpBddTI op gHigh1 gHigh2 (ht4, i + 1)


    restrictBddTI :: BddTI -> (BddTableIndex, BoolVal) -> BddTI -> BddTI
    restrictBddTI ([], _) _ _ = error "BddTable.restrictBddTI: BddTable empty"
    restrictBddTI g (j, v) (h, i)
            | elem u [0, 1]         = (h, u)
            | varU < j              = makeBddTableNode gr (varU, lv, hv)
            | varU > j              = makeBddTableNode gr (varU - 1, lv, hv)
            | otherwise             = if v then restrictBddTI gHigh (j, v) (h, i + 1)
                                            else restrictBddTI gLow (j, v) (h, i + 1) where
                    (((_, e):_), u) = g
                    (varU, lowU, highU) = if isNothing e then error "BddTable.restrictBddTI: table empty" else fromJust e
                    gLow = getBddTI g lowU
                    gHigh = getBddTI g highU
                    (ht, lv) = restrictBddTI gLow (j, v) (h, i + 1)
                    (gr, hv) = restrictBddTI gHigh (j, v) (ht, i + 1)

{-

    isIsomorphicBddTI :: BddTI -> BddTI -> Bool
    isIsomorphicBddTI (_, 0) (_, 0) = True
    isIsomorphicBddTI (_, 1) (_, 1) = True
    isIsomorphicBddTI (_, 0) (_, 1) = False
    isIsomorphicBddTI (_, 1) (_, 0) = False
    isIsomorphicBddTI g1 g2 =
            (v1 == v2)
            && isIsomorphicBddTI (getBddTI g1 l1) (getBddTI g2 l2)
            && isIsomorphicBddTI (getBddTI g1 h1) (getBddTI g2 h2) where
                    (((_, e1):_), _) = g1
                    (((_, e2):_), _) = g2
                    (v1, l1, h1) = if isNothing e1 then error "BddTable.isIsomorphicBddTI: table 1 empty"
                                            else fromJust e1
                    (v2, l2, h2) = if isNothing e2 then error "BddTable.isIsomorphicBddTI: table 2 empty"
                                            else fromJust e2


    showBddTI :: BddTI -> Maybe [BoolVar] -> String
    showBddTI (ts, i) order = titleStr ++ (showTable ts) ++ "Root: " ++ (show i) where
            titleStr = (nS $ col0+col01) ++ "var" ++ (nS col12) ++ "low" ++ (nS col23) ++ "high" ++ "\n"
            showTable :: BddTable -> String
            showTable [] = ""
            showTable (t:ts) = (showTable ts) ++ (showEntry t) ++ "\n"
            showEntry :: (BddTableIndex, Maybe BddTableEntry) -> String
            showEntry e
                    | isNothing $ snd e     = printColumn (show $ fst e) col0
                    | otherwise             = (printColumn (show $ fst e) col0) ++ (nS col01)
                                            ++ (showBddTableEntry $ fromJust $ snd e)
            showBddTableEntry :: BddTableEntry -> String
            showBddTableEntry (a, b, c) = var ++ (nS col12)
                                            ++ (printColumn (show b) col2) ++ (nS col23)
                                            ++ (printColumn (show c) col3) where
                    var = if (isNothing order) then (printColumn (show a) col1)
                            else (printColumn (head $ drop (a - 1) (fromJust order)) col1)
            printColumn :: String -> Int -> String
            printColumn e w = (nS (w - (length e))) ++ e
            col01 = 2; col12 = 2; col23 = 1
            col0 = 4; col1 = 3; col2 = 3; col3 = 4
-}

    maxVI::BoolExpr -> Int
    maxVI t
      = case (t) of
            Val v           -> 0 
            Var x           -> n2i x
            Not x           -> maxVI x
            And x y         -> max (maxVI x)(maxVI y)
            Or x y          -> max (maxVI x)(maxVI y)
            Imply x y       ->  max (maxVI x)(maxVI y)
            BiImply x y     ->  max (maxVI x)(maxVI y)

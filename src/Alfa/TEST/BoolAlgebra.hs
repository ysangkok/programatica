module BoolAlgebra (BoolOp(..), BoolExpr(..), BoolVal, BoolVar, rewriteBoolExpr, totalEvalBoolExpr,
                            getBoolVal, showBoolExpr, applyBoolOp, toBoolVal, toBv, bvZero, bvOne) where
    import GenNat --(Nat)
    type BoolVar = Nat --Int --String
    type BoolVal = Bool
    --
    bvZero = False
    bvOne = True
    --

    data BoolOp = BoolNot | BoolAnd | BoolOr | BoolImply | BoolBiImply              deriving (Show, Eq)

    data BoolExpr
      =     Var BoolVar                     |
            Val BoolVal                     |
            Not BoolExpr                    |
            And BoolExpr BoolExpr           |
            Or BoolExpr BoolExpr            |
            Imply BoolExpr BoolExpr         |
            BiImply BoolExpr BoolExpr                       deriving Show

    type BoolAssign = (BoolVal, BoolVar)

    --
    -- Boolean Algebra Not-Reduction Rules
    -- 
    -- ~(x ^ y) = (~x v ~y)
    -- ~(x v y) = (~x ^ ~y)
    -- ~(x => y) = (x ^ ~y)
    -- ~(x <=> y) = (x v y) ^ (~x ^ ~y))
    --

    -- Main Functions 
    reduceBoolExpr :: BoolExpr -> [BoolAssign] -> BoolExpr
    reduceBoolExpr t [] = t
    reduceBoolExpr t ((v,x):as) = reduceBoolExpr (evaluateBoolExpr $ reduceOnce t (v,x)) as

    reduceOnce :: BoolExpr -> BoolAssign -> BoolExpr
    reduceOnce t (val,xpr)
      = case (t) of
            Var x           -> if (x == xpr) then (Val val) else (Var x) 
            Val v           -> Val v
            Not x           -> Not (evaluateBoolExpr $ reduceOnce x (val,xpr))
            And x y         -> And (evaluateBoolExpr $ reduceOnce x (val,xpr)) (evaluateBoolExpr $ reduceOnce y (val,xpr))
            Or x y          -> Or (evaluateBoolExpr $ reduceOnce x (val,xpr)) (evaluateBoolExpr $ reduceOnce y (val,xpr))
            Imply x y       -> Imply (evaluateBoolExpr $ reduceOnce x (val,xpr)) (evaluateBoolExpr $ reduceOnce y (val,xpr))
            BiImply x y     -> BiImply (evaluateBoolExpr $ reduceOnce x (val,xpr)) (evaluateBoolExpr $ reduceOnce y (val,xpr))

    evaluateBoolExpr :: BoolExpr -> BoolExpr
    evaluateBoolExpr t
      = case (t) of
            Var x                   -> Var x
            Val v                   -> Val v
            Not (Val v)             -> Val (not v)
            Not (Not x)             -> x
            Not x                   -> Not x
            And (Val v) y           -> if v then y else (Val bvZero)
            And x (Val v)           -> if v then x else (Val bvZero)
            And x y                 -> And x y
            Or (Val v) y            -> if v then (Val bvOne) else y
            Or x (Val v)            -> if v then (Val bvOne) else x
            Or x y                  -> Or x y
            Imply (Val v) y         -> if v then y else (Val bvOne)
            Imply x (Val v)         -> if v then (Val bvOne) else evaluateBoolExpr (Not x)
            Imply x y               -> Imply x y
            BiImply (Val v) y       -> if v then y else evaluateBoolExpr (Not y)
            BiImply x (Val v)       -> if v then x else evaluateBoolExpr (Not x)
            BiImply x y             -> BiImply x y

    -- Replacement Boolean Reduction Functions

    rewriteBoolExprs :: BoolExpr -> [BoolAssign] -> BoolExpr
    rewriteBoolExprs t [] = t
    rewriteBoolExprs t (a:as) = rewriteBoolExprs (rewriteBoolExpr t a) as

    rewriteBoolExpr :: BoolExpr -> BoolAssign -> BoolExpr
    rewriteBoolExpr t (val, xpr)
      = case (t) of
            Var x           -> if (x == xpr) then (Val val) else (Var x) 
            Val v           -> Val v
            Not x           -> Not (rewriteBoolExpr x (val,xpr))
            And x y         -> And (rewriteBoolExpr x (val,xpr)) (rewriteBoolExpr y (val,xpr))
            Or x y          -> Or (rewriteBoolExpr x (val,xpr)) (rewriteBoolExpr y (val,xpr))
            Imply x y       -> Imply (rewriteBoolExpr x (val,xpr)) (rewriteBoolExpr y (val,xpr))
            BiImply x y     -> BiImply (rewriteBoolExpr x (val,xpr)) (rewriteBoolExpr y (val,xpr))

    totalEvalBoolExpr :: BoolExpr -> BoolVal
    totalEvalBoolExpr t
      = case (t) of
            Var x                   -> error "BoolAlgebra.totalEvalBoolExpr: variable still present in expression"
            Val v                   -> v
            Not (Val v)             -> if v then bvZero else bvOne
            Not (Not x)             -> (totalEvalBoolExpr x) 
            Not x                   -> not $ totalEvalBoolExpr x
            And (Val v) y           -> if v then (totalEvalBoolExpr y) else bvZero
            And x (Val v)           -> if v then (totalEvalBoolExpr x) else bvZero
            And x y                 -> (totalEvalBoolExpr x) && (totalEvalBoolExpr y)
            Or (Val v) y            -> if v then bvOne else (totalEvalBoolExpr y)
            Or x (Val v)            -> if v then bvOne else (totalEvalBoolExpr x)
            Or x y                  -> (totalEvalBoolExpr x) || (totalEvalBoolExpr y)
            Imply (Val v) y         -> if v then (totalEvalBoolExpr y) else bvOne
            Imply x (Val v)         -> if v then bvOne else (not $ totalEvalBoolExpr x)
            Imply x y               -> if (totalEvalBoolExpr x) then (totalEvalBoolExpr y) else bvOne
            BiImply (Val v) y       -> if v then (totalEvalBoolExpr y) else (not $ totalEvalBoolExpr y)
            BiImply x (Val v)       -> if v then (totalEvalBoolExpr x) else (not $ totalEvalBoolExpr x)
            BiImply x y             -> if (totalEvalBoolExpr x) then (totalEvalBoolExpr y) else (not $ totalEvalBoolExpr y)


    getBoolVal :: BoolExpr -> BoolVal
    getBoolVal (Val v) = v
    getBoolVal _ = error "BoolAlgebra.getBoolVal: not a value"

    showBoolExpr :: BoolExpr -> String
    showBoolExpr t
      = case (t) of
            Val v           -> if v then "1" else "0"
            Var x           -> show x
            Not x           -> "~" ++ showBoolExpr x
            And x y         -> "(" ++ (showBoolExpr x) ++ " ^ " ++ (showBoolExpr y) ++ ")"
            Or x y          -> "(" ++ (showBoolExpr x) ++ " v " ++ (showBoolExpr y) ++ ")"
            Imply x y       -> "(" ++ (showBoolExpr x) ++ " => " ++ (showBoolExpr y) ++ ")"
            BiImply x y     -> "(" ++ (showBoolExpr x) ++ " <=> " ++ (showBoolExpr y) ++ ")"

    applyBoolOp :: BoolOp -> BoolVal -> BoolVal -> BoolVal
    applyBoolOp op v1 v2
      = case (op) of
            BoolNot         -> error "BoolAlgebra.applyBoolOp: Not binary"
            BoolAnd         -> getBoolVal $ evaluateBoolExpr (And (Val v1) (Val v2))
            BoolOr          -> getBoolVal $ evaluateBoolExpr (Or (Val v1) (Val v2))
            BoolImply       -> getBoolVal $ evaluateBoolExpr (Imply (Val v1) (Val v2))
            BoolBiImply     -> getBoolVal $ evaluateBoolExpr (BiImply (Val v1) (Val v2))


    toBv = toBoolVal
    toBoolVal :: Int -> BoolVal
    toBoolVal i
            | i == 0        = bvZero
            | i == 1        = bvOne
            | otherwise     = error "BoolAlgebra.toBool: not 0 or 1"


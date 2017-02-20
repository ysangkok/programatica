module Native where
import Literal
import ProofMonad
import Char
import Monads(traceM)

{-
nativeOps :: [String]
nativeOps = ["+","-","*","/", "==","/=",">=","<=",">","<"]

isNativeOp :: String -> Bool
isNativeOp s = s `elem` nativeOps 
-}


                         


evalNativeOp :: String -> [Literal] -> Maybe Literal
-- lacking test on LNative, LImpossible, LNoMatch

evalNativeOp "+" [LInteger i, LInteger i'] = return $ LInteger $ i + i'
evalNativeOp "+" [LRational f, LRational f'] = return $ LRational $ f + f'

evalNativeOp "-" [LInteger i, LInteger i'] = return $ LInteger $ i - i'
evalNativeOp "-" [LRational f, LRational f'] = return $ LRational $ f - f'

evalNativeOp "*" [LInteger i, LInteger i'] = return $ LInteger $ i * i'
evalNativeOp "*" [LRational f, LRational f'] = return $ LRational $ f * f'


evalNativeOp "/" [LRational i, LRational i'] = return $ LRational $ i / i'



evalNativeOp "divSafe" [LInteger i, LInteger i'] = return $ LInteger $ i `div` i'


evalNativeOp "negate" [LInteger i] = return $ LInteger $ - i
evalNativeOp "negate" [LRational f] = return $ LRational $ - f

evalNativeOp "quot" [LInteger i, LInteger i'] = return $ LInteger $ i `quot` i
evalNativeOp "rem" [LInteger i, LInteger i'] = return $ LInteger $ i `rem` i'
evalNativeOp "div" [LInteger i, LInteger i'] = return $ LInteger $ i `div` i'

evalNativeOp "==" [l,l'] = return $ LBool (l==l')
evalNativeOp "/=" [l,l'] = return $ LBool (l/=l')

evalNativeOp "<"  [LInteger i, LInteger i'] = return $ LBool (i < i')
evalNativeOp "<"  [LRational f, LRational f'] = return $ LBool (f < f')
evalNativeOp "<"  [LChar c, LChar c'] = return $ LBool (c < c')
evalNativeOp "<"  [LString s, LString s'] = return $ LBool (s < s')


evalNativeOp ">"  [LInteger i, LInteger i'] = return $ LBool (i > i')
evalNativeOp ">"  [LRational f, LRational f'] = return $ LBool (f > f')
evalNativeOp ">"  [LChar c, LChar c'] = return $ LBool (c > c')
evalNativeOp ">"  [LString s, LString s'] = return $ LBool (s > s')


evalNativeOp "<="  [LInteger i, LInteger i'] = return $ LBool (i <= i')
evalNativeOp "<="  [LRational f, LRational f'] = return $ LBool (f <= f')
evalNativeOp "<="  [LChar c, LChar c'] = return $ LBool (c <= c')
evalNativeOp "<="  [LString s, LString s'] = return $ LBool (s <= s')

evalNativeOp ">="  [LInteger i, LInteger i'] = return $ LBool (i >= i')
evalNativeOp ">="  [LRational f, LRational f'] = return $ LBool (f >= f')
evalNativeOp ">="  [LChar c, LChar c'] = return $ LBool (c >= c')
evalNativeOp ">="  [LString s, LString s'] = return $ LBool (s >= s')

evalNativeOp "show" [LInteger i] = return.LString $ show i
evalNativeOp "show" [LRational f] = return.LString $ show f
evalNativeOp "show" [LBool b] = return.LString $ show b
evalNativeOp "show" [LChar c] = return.LString $ show c
evalNativeOp "show" [LString s] = return.LString $ show s

evalNativeOp "chr" [LInteger i] = return.LChar $ chr ((read (show i))::Int)
evalNativeOp "ord" [LChar c] = return . LInteger . toInteger $ ord c
evalNativeOp "isAlpha" [LChar c] = return.LBool $ isAlpha c
evalNativeOp "isAlphaNum" [LChar c] = return.LBool $ isAlphaNum c
evalNativeOp "isDigit" [LChar c] = return.LBool $ isDigit c
evalNativeOp "isLower" [LChar c] = return.LBool $ isLower c
evalNativeOp "isSpace" [LChar c] = return.LBool $ isSpace c
evalNativeOp "isUpper" [LChar c] = return.LBool $ isUpper c
evalNativeOp "toUpper" [LChar c] = return.LChar $ toUpper c

evalNativeOp _ _ = Nothing
{-
evalNativeOps EqLit [l,l']  = Just $ LBool $ l==l'
evalNativeOps NEqLit [l,l'] = Just $ LBool $ l/=l'
evalNativeOps LteLit [l,l'] = Just $ LBool $ l <= l'
evalNativeOps GteLit [l,l'] = Just $ LBool $ l >= l'
evalNativeOps LtLit [l,l'] = Just $ LBool $ l < l'
evalNativeOps GtLit [l,l'] = Just $ LBool $ l > l'
evalNativeOps PlusLit [LInteger i, LInteger i'] = Just $ LInteger $ i + i'
evalNativeOps MinusLit [LInteger i, LInteger i'] = Just $ LInteger $ i - i'
evalNativeOps TimesLit [LInteger i, LInteger i'] = Just $ LInteger $ i * i'
evalNativeOps DivideLit [LInteger i, LInteger i'] = 
   if i' /= 0 then Just $ LInteger $ i - i' else Nothing
evalLit NegateLit [LInteger i] = Just $ LInteger $ - i 
-}


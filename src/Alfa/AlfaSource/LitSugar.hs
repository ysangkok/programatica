module LitSugar(desugarChar,desugarString,desugarInt,desugarRat,sugarLit) where
import Char(isHexDigit,isDigit)
import Maybe(fromMaybe)
import Monad(MonadPlus(..))
import Ratio(numerator,denominator,(%))
import UAbstract
import AbstractOps
--import Debug2

f # x =  fmap f x

desugarChar c = con2 "C" (n (i `div` 16)) (n (i `mod` 16))
  where
    i = fromEnum c
    n d = con0 ['N',hex d]

desugarString [] = eNil
desugarString (c:cs) = eCons (desugarChar c) (desugarString cs)

desugarRat r = con2 ":%" (desugarInt (numerator r)) (desugarInt (denominator r))

desugarInt n = ePair eSign (eDigits (show (abs n)))
  where
    eDigits = foldr (eCons . eDigit) eNil
    eDigit = con0 . (\d->['D',d])
    eSign = if n<0 then con0 "Neg" else con0 "Pos"

con0 = eCon0 . Con
con2 = eCon2 . Con
eNil = con0 "Nil"
eCons = con2 "Cons"
ePair = con2 "Pair"

sugarLit = underAnnots sugarLit'

{-
sugarLit' e =
    seq (e'==e') $
    trace ("sugarLit "++show e ++ " = "++show e') $
    e'
  where e' = sugarLit'' e
-}

sugarLit' e =
  fromMaybe e $
  (EChar # isChar e)
  `mplus` (EString # isString e)
  `mplus` (EInt # isInt e)
  `mplus` (ERat # isRat e)

isString e =
  do (ex,exs) <- isCons e
     c <- isChar ex
     s <- isString0 exs
     return (c:s)

isString0 e =
  isEString e 
  `mplus`
  isNil e
  `mplus`
  do (ex,exs) <- isCons e
     c <- isChar ex
     s <- isString0 exs
     return (c:s)

isNil (ECon (Con "Nil")) = Just []
isNil _ = Nothing


isChar e =
  case e of
    EChar c -> Just c
    ECon (Con "C") `EApp` n1 `EApp` n2 ->
      case (stripAnnots n1,stripAnnots n2) of
        (ECon (Con ['N',h]),ECon (Con ['N',l]))
	  | isHexDigit h && isHexDigit l
	  -> Just (toEnum (16*unhex h+unhex l))
	_ -> Nothing
    _ -> Nothing

isEString (EString s) = Just s
isEString _ = Nothing

hex d = "0123456789ABCDEF"!!d

unhex c = if c<='9'
	  then fromEnum c-fromEnum '0'
	  else fromEnum c-fromEnum 'A'+10

underAnnots f e =
  case splitAnnots e of
    (as,e') -> attachAnnots as (f e')

---

isCons = isCon2 "Cons"
isPair = isCon2 "Pair"

isCon2 s (ECon (Con s') `EApp` e1 `EApp` e2) | s'==s = Just (stripAnnots e1,stripAnnots e2)
isCon2 s _ = Nothing

isRat e =
  do (en,ed) <- isCon2 ":%" e
     n <- isInt en
     d <- isInt ed
     return (n % d)

isInt (EInt n) = Just n
isInt e =
  do (esgn,eds) <- isPair e
     sgn <- isSign esgn
     ds <- isDigits eds
     return (sgn*(if null ds then 0 else read ds))

isSign (ECon (Con "Neg")) = Just (-1)
isSign (ECon (Con "Pos")) = Just 1
isSign _ = Nothing

isDigits e =
  isNil e
  `mplus`
  do (d,ds) <- isCons e
     d <- isDigit' d
     ds <- isDigits ds
     return (d:ds)

isDigit' (ECon (Con ['D',d])) | isDigit d = Just d
isDigit' _ = Nothing

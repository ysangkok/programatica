module EditMonad(
  EdM,returnEd,bindEd,thenEd,
  errorEd,handleEd,caseEd,
  storeEd,loadEd,updateEd,
  mapEd,mapStateEd,
  liftStateEd,
  runEd,edM,
  liftMaybeEd,liftEitherEd
  ) where

#ifdef __HASKELL98__
#define map fmap
#endif

newtype EdM s e a = Ed (s -> Either e (a,s))

instance Show (EdM s e a) where	-- for debugging only
  show _ = "Ed <<function>>"

instance Monad (EdM s e) where
  (>>=) = bindEd
  return = returnEd

instance Functor (EdM s e) where
  map = mapEd

runEd (Ed ed) = ed
edM = Ed

returnEd x = Ed (\s->Right (x,s))

storeEd s = Ed $ \ _ -> Right ((),s)
loadEd = Ed $ \ s -> Right (s,s)
updateEd upd = Ed $ \ s -> Right ((),upd s)

(Ed ed1) `bindEd` xed2 =
  Ed $ \ s -> case ed1 s of
                Left e -> Left e
		Right (x,s') -> runEd (xed2 x) s'

ed1 `thenEd` ed2 = ed1 `bindEd` const ed2

errorEd = Ed . const . Left

handleEd' (Ed ed) =
  Ed $ \s-> case ed s of
              Right (a,s') -> Right (Right a, s')
	      Left e -> Right (Left e,s)

caseEd ed okEd errEd =
  handleEd' ed `bindEd` \ x ->
  case x of
    Right a -> okEd a
    Left e -> errEd e

handleEd ed = caseEd ed returnEd

mapEd f ed = ed `bindEd` (returnEd.f)

mapStateEd f ed =
  ed `bindEd` \ x ->
  loadEd `bindEd` \ s ->
  returnEd (f s x)

liftStateEd proj inj (Ed ed) =
  Ed $ \s -> case ed (proj s) of
               Right (x,s') -> Right (x,inj s' s)
	       Left e -> Left e

liftMaybeEd e m =
  Ed $ \ s -> case m of
                Just x -> Right (x,s)
		_      -> Left e

liftEitherEd m =
  Ed $ \ s -> case m of
                Right x -> Right (x,s)
		Left e  -> Left e


module AlfEditKs where
import AllFudgets
import DialogueIO hiding (IOError)
--import AlfModules(FileName(..))
import AlfState(AlfState)
import EditMonad

#ifdef __HASKELL98__
#define map fmap
#endif

newtype AlfEdKs i o err ans = AEKs (Ks i o AlfState (Either err ans))

instance Monad (AlfEdKs i o err) where
  (>>=) = bindEdK
  return = unitEdK

instance Functor (AlfEdKs i o err) where
  map f edk = edk >>= return . f

edKs = AEKs . edKs'

edKs' :: EdM state err ans -> Ks i o state (Either err ans)
edKs' ed = mapKs (runEd ed) loadKs `bindKs`
          either (unitKs . Left)
		 (\(ans,s') -> storeKs s' `thenKs` unitKs (Right ans))

{-
-- These definitions require Fudgets 2003-12-10 or newer
toAEKs k = AEKs (Right `fmap` toMs k)
toAEKsc k = AEKs (Right `fmap` toMsc k)
-}

unitEdK'  x = unitKs (Right x)
errorEdK' x = unitKs (Left x)

unitEdK :: ans -> AlfEdKs i o err ans
unitEdK = AEKs . unitEdK'

errorEdK :: err -> AlfEdKs i o err ans
errorEdK = AEKs . errorEdK'

AEKs edk1 `bindEdK'` edk2 =
  AEKs $
  edk1 `bindKs` \ res ->
  case edk2 res of AEKs edk -> edk

bindEdK :: AlfEdKs i o err a -> ( a -> AlfEdKs i o err b) -> AlfEdKs i o err b
edk1 `bindEdK` edk2 =
  edk1 `bindEdK'` \ res ->
  case res of
    Left err -> errorEdK err
    Right res -> edk2 res

handleEdK :: AlfEdKs i o e ans -> (e->AlfEdKs i o err ans) -> AlfEdKs i o err ans
edk1 `handleEdK` edk2 =
  edk1 `bindEdK'` \ res ->
  case res of
    Left err -> edk2 err
    Right res -> unitEdK res

edk1 `thenEdK` edk2 = edk1 `bindEdK` const edk2


readFileEdK :: FilePath -> AlfEdKs i o String String
readFileEdK filename =
  AEKs $
  toMs (haskellIO (ReadFile filename)) `bindKs` \ resp ->
  case resp of
    Str s -> unitEdK' (rmcr s)
    Failure ioerr -> errorEdK' (show ioerr)

-- To be able to read DOS files in Unix:
rmcr "" = ""
rmcr ('\r':'\n':s) = '\n':rmcr s
rmcr (c:s) =c:rmcr s

--echoEdK str :: String -> AlfEdKs i o s ()
--echoEdK = toAEKsc . echoK

echoEdK str =
   AEKs $
   toMs (haskellIO (AppendChan "stdout" (str++"\n"))) `bindKs` \ resp ->
   unitEdK' ()

runEdK (AEKs edK) state =
  stateMonadK state
  (edK `bindKs` \ res -> 
   loadKs `bindKs` \ state' ->
   unitKs $
   case res of
     Left err -> Left err
     Right res -> Right (res,state'))

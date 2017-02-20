module UnfoldL(unfoldL,SemiFolded) where

import Eval
--import MiscId 
import Error
import PPrint
import Monads
import Literal
import ISynType
import ISynEnv(accessible)
import ISyntax(getExpPos)
import Id(UId)
import MiscId(trueId)

type SemiFolded = Value

unfoldL :: [UId] -- constants in scope
        -> Value 
        -> PCM SemiFolded

unfoldL lcs v0 = do k <- getTermCounter; evalMeta v0 >>= unf k [] where
  unf :: Int -> [(Bool,Value)] -> Value -> PCM Value
  unf k us v | k < 0     = terminationErr
             | otherwise = case v of
      EApp h vs -> do let (hs,vs') = unzip vs
                      us1 <- mapM (unf k []) vs'
                      unf k ((zip hs us1)++us) h
      EClos r (EConstV c xs)
        | c `elem` lcs -> return asis
        | otherwise -> def c >>= \d -> case rhsOfDef d of
            DExp e | isAbstract d && notElem c (accessible r) -> return asis
                   | otherwise -> eval' r e us >>= unf (k-1) [] >>= \v2 ->
                       case v2 of EStop m v3 -> vStop m$ appChoice v3 asis
                                  _          -> return $ appChoice v2 asis
            _ -> return asis
      EProj h l -> unf k [] h >>= prj >>= vap us where
        prj v1 = case v1 of
          EStop m v2 -> vStop m$ EProj v2 l
          EClos r v2 -> case v2 of {
            EStruct  _ _ xs cs _ -> go xs cs;
            Epackage _ _ xs cs _ -> go xs cs;
            _ -> return$ EProj v1 l } where
            go xs cs = lookupProj l cs >>= vConst r `flip` xs >>= unf k []
          _ -> return$ EProj v1 l 
        vap us1 (EClos r e ) = eval' r e us1 >>= unf k []
        vap us1 (EApp  h vs) = vap (vs++us1) h
        vap us1 v1           = return$ eApp' v1 us1
      EIf vb v1 v2 -> do vb' <- unf k [] vb
                         case vb' of
                                (ECon i [])  -> if i == trueId then unf k us v1 else unf k us v2
                                (EConF i _ []) -> if i == trueId then unf k us v1 else unf k us v2
                                (ELiteral _ (LBool b)) -> if b then unf k us v1 else unf k us v2
                                (EStop m v) -> vStop m (EIf vb v1 v2) >>= \v -> vApp v us
                                _ -> do v1' <- unf k us v1
                                        v2' <- unf k us v2
                                        return$ EIf vb' v1' v2'
      EClos r (ECase e cbe) -> eval r e >>= unf k [] >>= \v1 -> 
        if isStopped v1 then splitStop v1 >>= (`vStop` asis).fst
         else maybe (return asis)(\ (r2,e2)-> eval' r2 e2 us >>= unf k [])
              (matchAll r v1 cbe)
      _ -> return asis
    where
    asis = eApp' v us 
    terminationErr = raise$ eMsg(getExpPos v0)
                     (ETermination (ppReadable v0)(ppReadable v))

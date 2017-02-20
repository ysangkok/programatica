module TESTLazy(computeL, qiaoContext) where
import Agda(computeWHNF,getContext,ArgsBind(..),getUIdString)
import qualified ConvToAgda as CVA
import qualified ConvFromAgda as U
import UAbstract(Var(..))
import SubstMeta(recordSubstMeta)
import AgdaProofEngine(liftPCM,metaSubst)

computeL optg e =
 do e' <- liftPCM $ CVA.exp e >>= computeWHNF optg
    ms <- metaSubst
    return $ recordSubstMeta ms $ U.exp e'


qiaoContext m = concatMap conv `fmap` liftPCM(getContext m) where
  conv (AnArg x t  ) = [(getUIdString x, U.exp t, Nothing      )]
  conv (ABind x t e) = [(getUIdString x, U.exp t, Just(U.exp e))]
  conv _             = []



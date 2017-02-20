module AgsyPlugin where
import AgdaPluginKit
import Alfa.UAbstract
import qualified Alfa.ConvToAgda as CTA
import qualified Alfa.ConvFromAgda as CFA
import CSyntax
import Id(UId, toId)
import Maybe(fromJust)
import Alfa.Agda(extractMetaVars)


-- BasicOps defines:
-- data ArgsBind = AnArg UId CExpr | ABind UId CExpr CExpr | APackage UId
data UArgsBind = UArg Var Exp | UBind Var Exp Exp | UPackage Var
  deriving(Eq,Show)

type UMetaEnv = [(MetaVar, Exp)]
agsySearch :: MetaVar -> [UArgsBind] -> UMetaEnv -> Either String Exp

agsySearch meta binds metaenv
   = Left $ "Not implemented yet"
                ++ "\nMeta: "  ++ (show meta)
                ++ "\nContext: " ++ (show binds)
                ++ "\nGoal: " ++ (show $ lookup meta metaenv)


agsyPlugin :: Plugin
agsyPlugin = ("Agsy", "auto tactic",
                agsyDispatch)

xlatUId :: UId -> Var
xlatUId = CFA.var . toId

xlatArgsBind :: ArgsBind -> UArgsBind
xlatArgsBind (AnArg uid ce) = UArg (xlatUId uid) (CFA.exp ce)
xlatArgsBind (ABind uid ct ce) 
  = UBind (xlatUId uid) (CFA.exp ct) (CFA.exp ce)
xlatArgsBind (APackage uid) = UPackage(xlatUId uid)

xlatExp :: Exp -> PCM CExpr
xlatExp e = CTA.exp e


metaType :: MetaVar -> PCM Exp
metaType mv = do
		cj <- getTypeOfMeta 0 mv
                return $ CFA.exp (judge cj)
        where
           judge (HasType _ t) = t


metaPair :: MetaVar -> PCM (MetaVar, Exp)
metaPair mv = do mt <- metaType mv
                 return (mv, mt)

listMetaVars :: PCM [MetaVar]
listMetaVars = readSTM extractMetaVars

buildMetaEnv :: PCM UMetaEnv
buildMetaEnv = do
                 mvs <- listMetaVars
                 mapM metaPair mvs
               
                 
agsyDispatch :: PluginDispatch
agsyDispatch (GoalByNum 1 m) 
  = do
      binds <- liftPCMIM  $ getContext m 
      metaenv <- liftPCMIM buildMetaEnv
      let ubinds = map xlatArgsBind binds
      let res = agsySearch m ubinds metaenv
      case res of
               Left msg -> printAction msg
               Right exp -> liftPCMIM $ do
                  cexp <- xlatExp exp
                  giveme m cexp
                  return ()
      
                               

agsyDispatch _ =  printAction "Info plugin: no such function"

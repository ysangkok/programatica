module GetEnv(getenv,getenvdef,getEnvi) where
import IOUtil(getEnvi)
--import Maybe

getenv = getenvdef ""

getenvdef def var =
  case getEnvi var of
    Just s -> s
    _ -> def


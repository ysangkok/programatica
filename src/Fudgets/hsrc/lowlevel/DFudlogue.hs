module DFudlogue(Fudlogue,fudlogue, fudlogue', HasCache(..)) where

import FDefaults
import Fudlogue(fudlogue1)
import Fudget
import Xtypes
--import Cache(allcacheF)
import NewCache(allcacheF)
import CmdLineEnv(argFlag)
import DialogueIO(dialogueToIO)

#include "defaults.h"

newtype Fudlogue = Pars [Pars]
data Pars = Cache Bool

parameter_class(Cache,Bool)
parameter_instance(Cache,Fudlogue)

fudlogue = fudlogue' standard
fudlogue' :: (Customiser Fudlogue) -> F a b -> IO ()
fudlogue' pmod f = dialogueToIO $ fudlogue1 (cache f) where
   ps = pmod (Pars [Cache usecache])
   cache = if getCache ps then allcacheF else id


usecache = argFlag "cache" True

------------------------------------------------------------------------------
-- |
-- Module      : DynamicPlugin
-- Copyright   : (c) Daisuke Ikegami 2005
-- License     : Agda license (see src/../LICENSE in detail)
-- 
-- Maintainer  : ikegami-daisuke(at)aist.go.jp
-- Stability   : provisional
-- Portability : portable except Microsoft Windows
-- 
-- This module defines a dynamic plugin interface for Agda
-- based on "hs-plugins" <http://www.cse.unsw.edu.au/~dons/hs-plugins/>
--
------------------------------------------------------------------------------

{-
#include "config.h"
-}

module DynamicPlugin(
  -- | information of plugin
  PluginInfo(..),
  -- | run plugin
  runPluginCommand,
  -- | print information of plugin
  outputPluginMessage) 
where

import List(intersperse)
import Position(Position)
import MetaVars(MetaVar)
import Monads(raise)
import Error(ErrMsg(EPluginError))
import Plugins                      -- for hsplugins
import PluginAPI(GoalString, ResultString, InfoString, Interface(..))
import InteractionMonad(IM, liftIOIM, accessPCMIM)

{- | Information of dynamic plugin.
-}
data PluginInfo = PluginInfo 
    {
     pluginName :: String,     -- ^ plugin name
     pluginPath :: FilePath,   -- ^ where plugin is
     pluginCommand :: Symbol,  -- ^ plugin command
     pluginArgs :: [String]    -- ^ optional arguments
    } deriving (Eq, Show)

-- | file extension of dynamic loading object
objectExt :: String
objectExt = ".o"
-- | name of data of plugin interface 'PluginAPI.Interface'
pluginInterface :: String
pluginInterface = "PluginAPI.Interface"
-- | path of API file where defined by the configure script at installation
pluginInterfacePath :: FilePath
pluginInterfacePath = APIDIR -- depends on the configure (via cpp)

-- | run plugin command 
runPluginCommand :: Position    -- ^ position at agda files
                 -> MetaVar     -- ^ meta variable info
                 -> PluginInfo  -- ^ plugin info
                 -> GoalString  -- ^ string in the goal
  -- | result which must be replaced at goal, modified meta variables, and
  --   information which is displayed a plugin buffer on emacs
                 -> IM (ResultString, [MetaVar], InfoString)
runPluginCommand pos m pi s =
    do let args = pluginArgs pi
       z <- liftIOIM $ loadPlugin pi
       case z of
	    Just (_, a) -> agdaPlugin a pos m s args
	    Nothing     -> do let mes = "load error : " ++ 
					showPluginInfo pi
			      accessPCMIM $ raise (pos, EPluginError mes)

-- | helper function of 'runPluginCommand'. 
-- load plugin using hs-plugins pdynload
loadPlugin :: PluginInfo -> IO (Maybe (Module, a))
loadPlugin pi = 
    do let name = pluginName pi
           path = pluginPath pi
           comm = pluginCommand pi
           obj = path ++ "/" ++ name ++ objectExt
           apiPath = [".", pluginInterfacePath]
       pdynload obj apiPath [] pluginInterface comm

-- | helper function of 'runPluginCommand'.
showPluginInfo :: PluginInfo -> String
showPluginInfo pi = 
    let name = pluginName pi
        path = pluginPath pi
        comm = pluginCommand pi
        args = pluginArgs pi
    in "[plugin: " ++ name ++ " " ++
           "path: " ++ path ++ " " ++
           "command: " ++ comm ++ " " ++
           "args: " ++ foldr (++) "" (intersperse " " args) ++ "]"

-- | a buffer name on Emacs for dynamic plugin
pluginBuffer :: String
pluginBuffer = "* Agda Plugin *"

-- | print message of plugin
outputPluginMessage :: String -> IM ()
outputPluginMessage s = liftIOIM $ putStr $ infoCommand pluginBuffer s

-- borrowed from emacsagda.hs
-- these copy/paste cheat is quite bad manner...
inPar :: String -> String
inPar s = " (" ++ s ++ ")"

mkString :: String -> String
mkString s = show s

mkEmacsCommand :: String -> String
mkEmacsCommand s = "--- " ++ s ++ " +++\n"

mkCommand :: String -> String -> String
mkCommand s1 s2 = mkEmacsCommand (inPar (mkString s1 ++ inPar s2))

infoCommand s1 s2 = mkCommand "info" (mkString s1 ++ " " ++ mkString s2)

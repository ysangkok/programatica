------------------------------------------------------------------------------
-- |
-- Module      : PluginAPI
-- Copyright   : (c) Daisuke Ikegami 2005
-- License     : Agda license (see src/../LICENSE in detail)
-- 
-- Maintainer  : ikegami-daisuke(at)aist.go.jp
-- Stability   : provisional
-- Portability : portable except Microsoft Windows
-- 
-- This module defines a dynamic plugin API for Agda
-- based on "hs-plugins" <http://www.cse.unsw.edu.au/~dons/hs-plugins/>
--
------------------------------------------------------------------------------

module PluginAPI (
  -- | string at the goal
  GoalString, 
  -- | string which must be replaced at the goal
  ResultString, 
  -- | string which must be appeared at the plugin buffer on Emacs
  InfoString, 
  -- | data type of plugin API
  Interface(..), 
  -- | the body of plugin
  plugin)
where

import Position(Position)
import MetaVars(MetaVar)
import InteractionMonad(IM)

-- | string at the goal
type GoalString = String
-- | optional arguments for plugin
type Options = [String]
-- | string at the goal after running plugin
type ResultString = String
-- | information of plugin on another Emacs buffer
type InfoString = String

data Interface = 
    Interface { -- | the abstract body of plugin 
               agdaPlugin :: Position
                      -> MetaVar
                      -> GoalString
                      -> Options
                      -> IM (ResultString, [MetaVar], InfoString)
              }

plugin :: Interface
plugin = Interface { agdaPlugin = error "no plugin." }

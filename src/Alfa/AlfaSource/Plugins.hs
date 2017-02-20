module Plugins(module Plugins,loadModule,DeclsSource(..),Parsed,Annotations,unknownSourcePos,PluginName) where
import AlfaPlugin
-- import all plugins:
--import qualified TESTPlugin
import qualified DummyPlugin
--import qualified AgsyPlugin
--import qualified DerivePlugin
--import qualified GFPlugin
--import qualified AGFPlugin
--import qualified PescaPlugin ---AR
--import qualified OldGFPlugin
--import qualified TacPlugin

#include "exists.h"

type Plugins = [(PluginName,Plugin)]

data Plugin = EXISTS(state) Plugin EQV(state) (AlfaPlugin EQV(state))

initPlugins :: [FilePath] -> (PluginName->Bool) -> IO Plugins
initPlugins libDirs active = mapM init [p | p@(name,_)<-plugins,active name]
  where
    init (n,initplugin) =
      do putStrLn  ("* Activating plugin "++n)
         p <- initplugin libDirs
	 return (n,p)

plugins = [ --plug GFPlugin.plugin,
            --plug AGFPlugin.plugin,
	    --plug DerivePlugin.plugin,
            --plug PescaPlugin.plugin, ---AR
	    --plug OldGFPlugin.plugin,
	    -- add all plugins here
            -- plug TacPlugin.plugin,
	    --plug TESTPlugin.plugin,
	    --plug AgsyPlugin.plugin,
	    plug DummyPlugin.plugin
          ]

plug p = (name p,initPlugin p)
  where
    initPlugin p libDirs =
      do state <- state0 p libDirs
	 return (Plugin state p)
  

-- For debugging:
showPluginStates plugins =
  unlines [ name++": "++show (save m st) | (name,Plugin st m)<-plugins]

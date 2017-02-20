{- | Provides actual version number, and compilation date.
 
  The make file should include lines like

Version.o : force

        $(HC) -c -cpp Version.hs
-}       


module Version (version, compiled) where

{-
#ifdef __GLASGOW_HASKELL__
#define s_COMPILER__ "ghc"++ show (__GLASGOW_HASKELL__ :: Int)
#endif
#ifdef __HBC__
#define s_COMPILER__ "hbc"
#endif
#ifdef __HUGS__
#define s_COMPILER__ "hugs"
#endif
#ifndef s_COMPILER__
-}
s_COMPILER__ :: String
s_COMPILER__ = "unknown"
{-
#endif
-}

-- | A string containing the version number.
version :: String
version = "Agda idata  --- the version info brought to you by Ilya."

-- | A string containing compiler and compilation date.
compiled:: String
compiled = '[':s_COMPILER__++"; built "++__DATE__++ ' ':__TIME__++"]"

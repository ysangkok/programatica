{-# LANGUAGE ForeignFunctionInterface, CPP #-}
module AuxTypes where

#ifdef __GLASGOW_HASKELL__
import Data.Ix
-- fromEnum bug workaround
#define IX , Ix
#else
#define IX
#endif

data Gravity = ForgetGravity |
               NorthWestGravity |
               NorthGravity |
               NorthEastGravity |
               WestGravity |
               CenterGravity |
               EastGravity |
               SouthWestGravity |
               SouthGravity |
               SouthEastGravity |
               StaticGravity 
               deriving (Eq, Ord, Show, Enum IX)

data ShapeKind = ShapeBounding | ShapeClip  deriving (Eq, Ord, Show, Enum IX)

data ShapeOperation = ShapeSet |
                      ShapeUnion |
                      ShapeIntersect |
                      ShapeSubtract |
                      ShapeInvert 
                      deriving (Eq, Ord, Show, Enum IX)

-- There already is an Ordering in the 1.3 Prelude
data Ordering' = Unsorted | YSorted | YXSorted | YXBanded
     deriving (Eq, Ord, Show, Enum IX)

type RmClass = String
type RmName = String
type RmQuery = (RmClass, RmName)
type RmSpec = [RmQuery]
type RmValue = String
type RmDatabase = Int

rmNothing = 0::Int

data Modifiers = Shift |
                 Lock |
                 Control |
                 Mod1 |
                 Mod2 |
                 Mod3 |
                 Mod4 |
                 Mod5 |
                 Button1 |
                 Button2 |
                 Button3 |
                 Button4 |
                 Button5 |
                 Any 
                 deriving (Eq, Ord, Show, Read, Enum IX)

clModifiers =
    [Shift, Lock, Control, Mod1, Mod2, Mod3, Mod4, Mod5,
     Button1, Button2, Button3, Button4, Button5]

data Button = AnyButton | Button Int  deriving (Eq, Ord, Show)

type ModState = [Modifiers]

type KeySym = String


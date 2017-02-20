{-# OPTIONS -fno-implicit-prelude #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Typeable
-- Copyright   :  (c) The University of Glasgow 2001
-- License     :  BSD-style (see the file libraries/base/LICENSE)
-- 
-- Maintainer  :  libraries@haskell.org
-- Stability   :  experimental
-- Portability :  portable
--
-- The Typeable class reifies types to some extent by associating type
-- representations to types. These type representations can be compared,
-- and one can in turn define a type-safe cast operation. To this end,
-- an unsafe cast is guarded by a test for type (representation)
-- equivalence. The module Data.Dynamic uses Typeable for an
-- implementation of dynamics. The module Data.Generics uses Typeable
-- and type-safe cast (but not dynamics) to support the \"Scrap your
-- boilerplate\" style of generic programming.
--
-----------------------------------------------------------------------------

module Data.Typeable
  (

	-- * The Typeable class
	Typeable( typeOf ),	-- :: a -> TypeRep

	-- * Type-safe cast
	cast,			-- :: (Typeable a, Typeable b) => a -> Maybe b
	castss,			-- a cast for kind "* -> *"
	castarr,		-- another convenient variation

	-- * Type representations
	TypeRep,	-- abstract, instance of: Eq, Show, Typeable
	TyCon,		-- abstract, instance of: Eq, Show, Typeable

	-- * Construction of type representations
	mkTyCon,	-- :: String  -> TyCon
	mkAppTy,	-- :: TyCon   -> [TypeRep] -> TypeRep
	mkFunTy,	-- :: TypeRep -> TypeRep   -> TypeRep
	applyTy,	-- :: TypeRep -> TypeRep   -> Maybe TypeRep

	-- * Observation of type representations
	typerepTyCon,	-- :: TypeRep -> TyCon
	typerepArgs,	-- :: TypeRep -> [TypeRep]
	tyconString	-- :: TyCon   -> String

  ) where

import qualified Data.HashTable as HT
import Data.Maybe
import Data.Either
import Data.Int
import Data.Word
import Data.List( foldl )

import Hugs.Prelude
import Hugs.IO
import Hugs.IORef
import Hugs.IOExts

	-- 
	-- let fTy = mkTyCon "Foo" in show (mkAppTy (mkTyCon ",,")
	--                                 [fTy,fTy,fTy])
	-- 
	-- returns "(Foo,Foo,Foo)"
	--
	-- The TypeRep Show instance promises to print tuple types
	-- correctly. Tuple type constructors are specified by a 
	-- sequence of commas, e.g., (mkTyCon ",,,,") returns
	-- the 5-tuple tycon.

----------------- Construction --------------------

-- | Applies a type constructor to a sequence of types
mkAppTy  :: TyCon -> [TypeRep] -> TypeRep
mkAppTy tc@(TyCon tc_k _) args 
  = TypeRep (appKeys tc_k arg_ks) tc args
  where
    arg_ks = [k | TypeRep k _ _ <- args]

funTc :: TyCon
funTc = mkTyCon "->"

-- | A special case of 'mkAppTy', which applies the function 
-- type constructor to a pair of types.
mkFunTy  :: TypeRep -> TypeRep -> TypeRep
mkFunTy f a = mkAppTy funTc [f,a]

-- | Applies a type to a function type.  Returns: @'Just' u@ if the
-- first argument represents a function of type @t -> u@ and the
-- second argument represents a function of type @t@.  Otherwise,
-- returns 'Nothing'.
applyTy :: TypeRep -> TypeRep -> Maybe TypeRep
applyTy (TypeRep _ tc [t1,t2]) t3
  | tc == funTc && t1 == t3	= Just t2
applyTy _ _     		= Nothing

-- If we enforce the restriction that there is only one
-- @TyCon@ for a type & it is shared among all its uses,
-- we can map them onto Ints very simply. The benefit is,
-- of course, that @TyCon@s can then be compared efficiently.

-- Provided the implementor of other @Typeable@ instances
-- takes care of making all the @TyCon@s CAFs (toplevel constants),
-- this will work. 

-- If this constraint does turn out to be a sore thumb, changing
-- the Eq instance for TyCons is trivial.

-- | Builds a 'TyCon' object representing a type constructor.  An
-- implementation of "Data.Typeable" should ensure that the following holds:
--
-- >  mkTyCon "a" == mkTyCon "a"
--

mkTyCon :: String	-- ^ the name of the type constructor (should be unique
			-- in the program, so it might be wise to use the
			-- fully qualified name).
	-> TyCon	-- ^ A unique 'TyCon' object
mkTyCon str = TyCon (mkTyConKey str) str

----------------- Observation ---------------------

-- | Observe the type constructor of a type representation
typerepTyCon :: TypeRep -> TyCon
typerepTyCon (TypeRep _ tc _) = tc

-- | Observe the argument types of a type representation
typerepArgs :: TypeRep -> [TypeRep]
typerepArgs (TypeRep _ _ args) = args

-- | Observe string encoding of a type representation
tyconString :: TyCon   -> String
tyconString  (TyCon _ str) = str

----------------- Showing TypeReps --------------------

instance Show TypeRep where
  showsPrec p (TypeRep _ tycon tys) =
    case tys of
      [] -> showsPrec p tycon
      [x]   | tycon == listTc -> showChar '[' . shows x . showChar ']'
      [a,r] | tycon == funTc  -> showParen (p > 8) $
			         showsPrec 9 a . showString " -> " . showsPrec 8 r
      xs | isTupleTyCon tycon -> showTuple tycon xs
	 | otherwise	     ->
	    showParen (p > 9) $
   	    showsPrec p tycon . 
	    showChar ' '      . 
	    showArgs tys

instance Show TyCon where
  showsPrec _ (TyCon _ s) = showString s

isTupleTyCon :: TyCon -> Bool
isTupleTyCon (TyCon _ (',':_)) = True
isTupleTyCon _		       = False

-- Some (Show.TypeRep) helpers:

showArgs :: Show a => [a] -> ShowS
showArgs [] = id
showArgs [a] = showsPrec 10 a
showArgs (a:as) = showsPrec 10 a . showString " " . showArgs as 

showTuple :: TyCon -> [TypeRep] -> ShowS
showTuple (TyCon _ str) args = showChar '(' . go str args
 where
  go [] [a] = showsPrec 10 a . showChar ')'
  go _  []  = showChar ')' -- a failure condition, really.
  go (',':xs) (a:as) = showsPrec 10 a . showChar ',' . go xs as
  go _ _   = showChar ')'

-------------------------------------------------------------
--
--	The Typeable class
--
-------------------------------------------------------------

-- | The class 'Typeable' allows a concrete representation of a type to
-- be calculated.
class Typeable a where
  typeOf :: a -> TypeRep
  -- ^ Takes a value of type @a@ and returns a concrete representation
  -- of that type.  The /value/ of the argument should be ignored by
  -- any instance of 'Typeable', so that it is safe to pass 'undefined' as
  -- the argument.

-------------------------------------------------------------
--
--		Type-safe cast
--
-------------------------------------------------------------

-- | The type-safe cast operation
cast :: (Typeable a, Typeable b) => a -> Maybe b
cast x = r
       where
	 r = if typeOf x == typeOf (fromJust r)
               then Just $ unsafeCoerce x
	       else Nothing

-- | A convenient variation for kind \"* -> *\"
castss :: (Typeable a, Typeable b) => t a -> Maybe (t b)
castss x = r
       where
	 r = if typeOf (get x) == typeOf (get (fromJust r))
               then Just $ unsafeCoerce x
	       else Nothing
         get :: t c -> c
	 get = undefined

-- | Another variation
castarr :: (Typeable a, Typeable b, Typeable c, Typeable d)
        => (a -> t b) -> Maybe (c -> t d)
castarr x = r
       where
	 r = if typeOf (get x) == typeOf (get (fromJust r))
               then Just $ unsafeCoerce x
	       else Nothing
         get :: (e -> t f) -> (e, f)
	 get = undefined

{-

The variations castss and castarr are arguably not really needed.
Let's discuss castss in some detail. To get rid of castss, we can
require "Typeable (t a)" and "Typeable (t b)" rather than just
"Typeable a" and "Typeable b". In that case, the ordinary cast would
work. Eventually, all kinds of library instances should become
Typeable. (There is another potential use of variations as those given
above. It allows quantification on type constructors.

-}

-------------------------------------------------------------
--
--	Instances of the Typeable class for Prelude types
--
-------------------------------------------------------------

listTc :: TyCon
listTc = mkTyCon "[]"

instance Typeable a => Typeable [a] where
  typeOf ls = mkAppTy listTc [typeOf ((undefined :: [a] -> a) ls)]
	-- In GHC we can say
	--	typeOf (undefined :: a)
	-- using scoped type variables, but we use the 
	-- more verbose form here, for compatibility with Hugs

unitTc :: TyCon
unitTc = mkTyCon "()"

instance Typeable () where
  typeOf _ = mkAppTy unitTc []

tup2Tc :: TyCon
tup2Tc = mkTyCon ","

instance (Typeable a, Typeable b) => Typeable (a,b) where
  typeOf tu = mkAppTy tup2Tc [typeOf ((undefined :: (a,b) -> a) tu),
			      typeOf ((undefined :: (a,b) -> b) tu)]

tup3Tc :: TyCon
tup3Tc = mkTyCon ",,"

instance ( Typeable a , Typeable b , Typeable c) => Typeable (a,b,c) where
  typeOf tu = mkAppTy tup3Tc [typeOf ((undefined :: (a,b,c) -> a) tu),
			      typeOf ((undefined :: (a,b,c) -> b) tu),
			      typeOf ((undefined :: (a,b,c) -> c) tu)]

tup4Tc :: TyCon
tup4Tc = mkTyCon ",,,"

instance ( Typeable a
	 , Typeable b
	 , Typeable c
	 , Typeable d) => Typeable (a,b,c,d) where
  typeOf tu = mkAppTy tup4Tc [typeOf ((undefined :: (a,b,c,d) -> a) tu),
			      typeOf ((undefined :: (a,b,c,d) -> b) tu),
			      typeOf ((undefined :: (a,b,c,d) -> c) tu),
			      typeOf ((undefined :: (a,b,c,d) -> d) tu)]
tup5Tc :: TyCon
tup5Tc = mkTyCon ",,,,"

instance ( Typeable a
	 , Typeable b
	 , Typeable c
	 , Typeable d
	 , Typeable e) => Typeable (a,b,c,d,e) where
  typeOf tu = mkAppTy tup5Tc [typeOf ((undefined :: (a,b,c,d,e) -> a) tu),
			      typeOf ((undefined :: (a,b,c,d,e) -> b) tu),
			      typeOf ((undefined :: (a,b,c,d,e) -> c) tu),
			      typeOf ((undefined :: (a,b,c,d,e) -> d) tu),
			      typeOf ((undefined :: (a,b,c,d,e) -> e) tu)]

instance (Typeable a, Typeable b) => Typeable (a -> b) where
  typeOf f = mkFunTy (typeOf ((undefined :: (a -> b) -> a) f))
		     (typeOf ((undefined :: (a -> b) -> b) f))

-------------------------------------------------------
--
-- Generate Typeable instances for standard datatypes
--
-------------------------------------------------------

boolTc = mkTyCon "Bool"; instance Typeable Bool where { typeOf _ = mkAppTy boolTc [] }
charTc = mkTyCon "Char"; instance Typeable Char where { typeOf _ = mkAppTy charTc [] }
floatTc = mkTyCon "Float"; instance Typeable Float where { typeOf _ = mkAppTy floatTc [] }
doubleTc = mkTyCon "Double"; instance Typeable Double where { typeOf _ = mkAppTy doubleTc [] }
intTc = mkTyCon "Int"; instance Typeable Int where { typeOf _ = mkAppTy intTc [] }
integerTc = mkTyCon "Integer"; instance Typeable Integer where { typeOf _ = mkAppTy integerTc [] }
ratioTc = mkTyCon "Ratio"; instance Typeable a => Typeable (Ratio a) where {   typeOf x = mkAppTy ratioTc [typeOf ((undefined :: Ratio a -> a) x) ] }
eitherTc = mkTyCon "Either"; instance (Typeable a, Typeable b) => Typeable (Either a b) where {   typeOf x = mkAppTy eitherTc [typeOf ((undefined :: Either a b -> a) x), 			     typeOf ((undefined :: Either a b -> b) x)] }
ioTc = mkTyCon "IO"; instance Typeable a => Typeable (IO a) where {   typeOf x = mkAppTy ioTc [typeOf ((undefined :: IO a -> a) x) ] }
maybeTc = mkTyCon "Maybe"; instance Typeable a => Typeable (Maybe a) where {   typeOf x = mkAppTy maybeTc [typeOf ((undefined :: Maybe a -> a) x) ] }
orderingTc = mkTyCon "Ordering"; instance Typeable Ordering where { typeOf _ = mkAppTy orderingTc [] }
handleTc = mkTyCon "Handle"; instance Typeable Handle where { typeOf _ = mkAppTy handleTc [] }
ptrTc = mkTyCon "Ptr"; instance Typeable a => Typeable (Ptr a) where {   typeOf x = mkAppTy ptrTc [typeOf ((undefined :: Ptr a -> a) x) ] }
stablePtrTc = mkTyCon "StablePtr"; instance Typeable a => Typeable (StablePtr a) where {   typeOf x = mkAppTy stablePtrTc [typeOf ((undefined :: StablePtr a -> a) x) ] }

int8Tc = mkTyCon "Int8"; instance Typeable Int8 where { typeOf _ = mkAppTy int8Tc [] }
int16Tc = mkTyCon "Int16"; instance Typeable Int16 where { typeOf _ = mkAppTy int16Tc [] }
int32Tc = mkTyCon "Int32"; instance Typeable Int32 where { typeOf _ = mkAppTy int32Tc [] }
int64Tc = mkTyCon "Int64"; instance Typeable Int64 where { typeOf _ = mkAppTy int64Tc [] }

word8Tc = mkTyCon "Word8" ; instance Typeable Word8 where { typeOf _ = mkAppTy word8Tc [] }
word16Tc = mkTyCon "Word16"; instance Typeable Word16 where { typeOf _ = mkAppTy word16Tc [] }
word32Tc = mkTyCon "Word32"; instance Typeable Word32 where { typeOf _ = mkAppTy word32Tc [] }
word64Tc = mkTyCon "Word64"; instance Typeable Word64 where { typeOf _ = mkAppTy word64Tc [] }

tyconTc = mkTyCon "TyCon"; instance Typeable TyCon where { typeOf _ = mkAppTy tyconTc [] }
typeRepTc = mkTyCon "TypeRep"; instance Typeable TypeRep where { typeOf _ = mkAppTy typeRepTc [] }

ioRefTc = mkTyCon "IORef"; instance Typeable a => Typeable (IORef a) where {   typeOf x = mkAppTy ioRefTc [typeOf ((undefined :: IORef a -> a) x) ] }

---------------------------------------------
--
--		Internals 
--
---------------------------------------------

data KeyPr = KeyPr !Key !Key deriving( Eq )

hashKP :: KeyPr -> Int32
hashKP (KeyPr (Key k1) (Key k2)) = (HT.hashInt k1 + HT.hashInt k2) `rem` HT.prime

data Cache = Cache { next_key :: !(IORef Key),
		     tc_tbl   :: !(HT.HashTable String Key),
		     ap_tbl   :: !(HT.HashTable KeyPr Key) }

{-# NOINLINE cache #-}
cache :: Cache
cache = unsafePerformIO $ do
		empty_tc_tbl <- HT.new (==) HT.hashString
		empty_ap_tbl <- HT.new (==) hashKP
		key_loc      <- newIORef (Key 1) 
		return (Cache { next_key = key_loc,
				tc_tbl = empty_tc_tbl, 
				ap_tbl = empty_ap_tbl })

newKey :: IORef Key -> IO Key

newKey kloc = do { k@(Key i) <- readIORef kloc ;
		   writeIORef kloc (Key (i+1)) ;
		   return k }

mkTyConKey :: String -> Key
mkTyConKey str 
  = unsafePerformIO $ do
	let Cache {next_key = kloc, tc_tbl = tbl} = cache
	mb_k <- HT.lookup tbl str
	case mb_k of
	  Just k  -> return k
	  Nothing -> do { k <- newKey kloc ;
			  HT.insert tbl str k ;
			  return k }

appKey :: Key -> Key -> Key
appKey k1 k2
  = unsafePerformIO $ do
	let Cache {next_key = kloc, ap_tbl = tbl} = cache
	mb_k <- HT.lookup tbl kpr
	case mb_k of
	  Just k  -> return k
	  Nothing -> do { k <- newKey kloc ;
			  HT.insert tbl kpr k ;
			  return k }
  where
    kpr = KeyPr k1 k2

appKeys :: Key -> [Key] -> Key
appKeys k ks = foldl appKey k ks

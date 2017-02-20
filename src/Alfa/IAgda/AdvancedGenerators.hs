{-# OPTIONS -fglasgow-exts #-}

-- | Generators of type-correct terms, including tests that the
-- generators are correct.
--
-- Those generators which are parameterised on sizes yield values
-- whose sizes are approximately bounded by the given size.

module AdvancedGenerators where

import Data.List hiding (union)
import Debug.QuickCheck
import Data.Generics
import Control.Monad
import Control.Arrow
import Data.Set

------------------------------------------------------------------------
-- Types

-- Names.

type Name = String

-- Terms.

data Term
  = Var Name
  | App Term Term
  | Lam Name Term
    deriving (Eq, Show, Typeable, Data)

-- | Types.

data Type
  = Set
  | El Term
  | Arr Name Type Type -- ^ (x:a) -> b
    deriving (Eq, Show, Typeable, Data)

-- | Contexts. Stored with the rightmost element first.

type Context = [(Name, Type)]

------------------------------------------------------------------------
-- Generators, including tests

-- | A generator for names. A small set of names is used to ensure
-- many name clashes.

name :: Gen Name
name = sized (elements . flip take names . (*2) . (+1))
  where names = map (:[]) ['a' .. 'z']

-- | Given a non-empty context this generator yields one of the
-- variables which are in scope.

varInContext :: Context -> Gen (Name, Type)
varInContext = elements . varsInScope
  where
  varsInScope :: Context -> Context
  varsInScope = nubBy (\x y -> fst x == fst y)

-- | A generator for well-formed contexts.

context :: Gen Context
context = sized (context' [])
  where
  context' _   n | n < 0 = error "context: Negative size."
  context' ctx 0 = return ctx
  context' ctx n = do
    t <- typ ctx
    x <- name
    context' ((x, t) : ctx) (n-1)

prop_context = forAll context wellFormedContext

-- | Given a well-formed context, generates a valid type.

typ :: Context -> Gen Type
typ = sized . flip typ'

-- | Given a size and a well-formed context, generates a valid type.

typ' :: Int -> Context -> Gen Type
typ' n ctx | n < 0     = error "typ': Negative size."
           | n == 0    = set
           | otherwise = frequency [(1, set), (2, el), (2, arr)]
  where
  set = return Set
  el = do
    t <- setTerm' (n-1) ctx
    return (El t)
  arr = do
    x <- name
    a <- typ' (n `div` 2) ctx
    b <- typ' (n `div` 2) ((x, a) : ctx)
    return (Arr x a b)

prop_typ = forAll context $ \ctx -> forAll (typ ctx) $ \t ->
           validType ctx t

-- | Given a well-formed context, generates a type and a term having
-- that type.

term :: Context -> Gen (Term, Type)
term = sized . flip term'

-- | Given a size and a well-formed context, generates a type and a
-- term having that type.

term' :: Int -> Context -> Gen (Term, Type)
term' n ctx | n < 0     = error "term': Negative size."
            | null ctx  = frequency [(1, lam), (1, app)]
            | n == 0    = var
            | otherwise = frequency [(1, var), (2, lam), (2, app)]
  where
  var = liftM (Var *** id) $ varInContext ctx

  lam = do
    x <- name
    a <- typ' (n `div` 2) ctx
    (t, b) <- term' (n `div` 2) ((x, a) : ctx)
    return (Lam x t, Arr x a b)

  app = do
    len <- choose (1, n `div` 4)
    ttns <- args len ctx
    (t, b) <- arrTerm' (n `div` 2) ctx ttns
    let (terms, types, names) = unzip3 ttns
    return ( foldr (flip App) t terms  -- t u1 u2 ... um.
           , foldr (\(u, x) ty -> subst u x ty) b $ zip terms names
             -- b[u1/x1][u2/x2]...[um/xm]
           )

  -- Generates a list of triples (ui, ai, xi) satisfying
  --   ctx |- u1 : a1
  --   ctx |- u2 : a2[u1 / x1, x2]
  --   ...
  --   ctx |- um : am[u1 / x1, xm]...[u(m-1) / x(m-1), xm]
  -- [t / x, y]: Substitute t for x, unless x = y.
  -- (u1, a1, x1) is the first element of the returned list.

  -- TODO: Implement correctly.
  args m _   | m < 0 = error "term': Internal error."
  args 0 ctx = return []
  args m ctx = do
    (u, a) <- term' (n `div` 2) ctx
    x <- name
    return [(u, a, x)]
    -- liftM ((u, a, x) :) $ args (m-1)

prop_term = forAll context $ \ctx -> forAll (term ctx) $ \(t, ty) ->
            typeCheck ctx t ty

-- | Given a well-formed context, generates a term of type 'Set'.

setTerm :: Context -> Gen Term
setTerm = sized . flip setTerm'

-- | Given a size and a well-formed context, generates a term of type
-- 'Set'.

setTerm' :: Int -> Context -> Gen Term
setTerm' n ctx = undefined

prop_setTerm = forAll context $ \ctx -> forAll (setTerm ctx) $ \t ->
               typeCheck ctx t Set

-- | Given a list @triples@ of triples @(ui, ai, xi)@ satisfying
-- >  ctx |- u1 : a1
-- >  ctx |- u2 : a2[u1 / x1, x2]
-- >  ...
-- >  ctx |- um : am[u1 / x1, xm]...[u(m-1) / x(m-1), xm]
-- @'arrTerm' ctx triples@ generates a term @t@ and a type @b@ such that
-- @ctx |- t : (x1:a1) -> (x2:a2) -> ... -> (xm:am) -> b@.

-- TODO: Currently does not generate variables.

arrTerm :: Context -> [(Term, Type, Name)] -> Gen (Term, Type)
arrTerm ctx triples = sized $ \n -> arrTerm' n ctx triples

-- | 'arrTerm'' is like 'arrTerm', but parameterised on a size.

arrTerm' :: Int -> Context -> [(Term, Type, Name)] -> Gen (Term, Type)
arrTerm' n ctx triples | n < 0        = error "arrTerm': Negative size."
                       | null triples = term' n ctx
                       | otherwise    = lam -- We should test if a
                                            -- variable could be returned. 
  where
  (_u, a, x) : rest = triples

  lam = do
    (t, ty) <- arrTerm' (n-1) ((x, a) : ctx) rest
    return (Lam x t, ty)

-- TODO: Give a property here. (Make the args generator visible.)

-- prop_arrTerm = forAll context $ \ctx ->
--                forAll (term ctx) $ \(u, a) ->
--                forAll name $ \x ->
--                forAll (arrTerm ctx x a) $ \(t, b) ->
--                typeCheck ctx t (Arr x a b)

------------------------------------------------------------------------
-- Substitution

class Subst a where
  -- | @'subst' t x a@ substitutes @t@ for @x@ in @a@.
  subst :: Term -> Name -> a -> a

instance Subst Type where
  subst t x ty = case ty of
    Set -> Set
    El u -> El $ subst t x u
    Arr y a b | x == y    -> ty
              | otherwise -> Arr y (subst t x a) (subst t x b)

instance Subst Term where
  subst t x u = case u of
    Var y | x == y    -> t
          | otherwise -> u
    App v w -> App (subst t x v) (subst t x w)
    Lam y v | x == y    -> u
            | otherwise -> Lam z (subst t x (subst (Var z) y v))
              where z = fresh (App t v)

-- | Given a term, yields a name which does not correspond to a free
-- variable in the term.

fresh :: Term -> Name
fresh t = case setToList (freeVars t) of
  [] -> "a"
  vs -> (++ "'")
        . fst
        . maximumBy (\x y -> compare (snd x) (snd y))
        . map (id &&& length)
        $ vs

-- | Yields all the free variables in the term.

freeVars :: Term -> Set Name
freeVars = freeVars'
  where
  freeVars' t = case t of
    Var v     -> unitSet v
    App t1 t2 -> freeVars' t1 `union` freeVars' t2
    Lam x t   -> delFromSet (freeVars' t) x

-- TODO: Test the substitution functions.

------------------------------------------------------------------------
-- Checks (type-checking)

-- | A check for well-formed contexts.

wellFormedContext :: Context -> Bool
wellFormedContext = all (\((_, t) : ctx) -> validType ctx t) . init . tails

-- | A check for valid types.

validType :: Context -> Type -> Bool
validType ctx typ = case typ of
  Set       -> wellFormedContext ctx
  El t      -> typeCheck ctx t Set
  Arr x a b -> validType ((x, a) : ctx) b

-- | @'typeCheck' ctx t ty@ returns 'True' iff @ctx |- t : ty@.

-- This one of course needs to be written in a different way...

typeCheck :: Context -> Term -> Type -> Bool
typeCheck ctx t ty = case t of
  Var x   -> lookup x ctx == Just ty
  Lam x t -> case ty of
    Arr y a b -> x == y && typeCheck ((x, a) : ctx) t b
    _         -> False
  App t u -> undefined

------------------------------------------------------------------------
-- Helper functions

-- | Generates a value satisfying the predicate.

untilP :: (a -> Bool) -> Gen a -> Gen a
untilP p gen = do
  a <- gen
  if p a then return a else untilP p gen

-- | Is the type an arrow type?

isArr :: Type -> Bool
isArr (Arr {}) = True
isArr _        = False

-- | Does the context contain a variable @a : Set@?

containsSetVar :: Context -> Bool
containsSetVar = any ((== Set) . snd)

-- | Generates a list of the specified length, using the generator.

listOfLen :: Int -> Gen a -> Gen [a]
listOfLen n gen = sequence $ replicate n gen

-- | Generates a list of arbitrary length, using the generator.

list :: Gen a -> Gen [a]
list gen = do
  n <- arbitrary
  listOfLen (abs n) gen

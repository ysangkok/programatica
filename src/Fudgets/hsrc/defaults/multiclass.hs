class HasDefault a d where
   setDef :: d -> a -> a

data Par a = Par Int a

instance HasDefault (Par a) a where
   setDef a (Par i _) = Par i a

class HasFont f a where
  setFont :: f -> a -> a

data ButtonArgs = BA String
instance HasFont String ButtonArgs where
   setFont s (BA _) = BA s

class FontArg f where
   fontName :: f -> String

instance FontArg String where
   fontName = id

instance FontArg Int where
   fontName = show

data Maybe a = Just a | Nothing
getpar pp [] = error "getpar:: missing default"
getpar pp (p:ps) = case pp p of
       Just a -> a
       Nothing -> getpar pp ps

class HasInitial v a where
   setInitial :: v -> a -> a
   getInitial :: a -> v

data Pars v = TextF v
data TextF v = Pars [Pars v]

instance HasInitial v (TextF v) where
  setInitial p (Pars ps) = Pars (TextF p:ps)
  getInitial (Pars ps) = getpar (\x->case x of TextF p -> Just p; _ -> Nothing) ps

--f1 :: (TextF v -> TextF v) -> v -> v
textF :: (Show c, HasInitial c (TextF c)) => (TextF c -> TextF c) -> c -> String
textF d e = show v where Pars [TextF v] = d (Pars [])

{-
--f :: TextArgs v -> String
{-
The type signatureE
   f :: (HasInitial c b,Text c) => (TextF a -> b) -> [Char]
gives
  ERROR "multiclass.hs" (line 43): Ambiguous type signature in type declaration
  *** ambiguous type : (HasInitial a b, Text a) => (TextF c -> b) -> [Char]
  *** assigned to    : f
but it's the type sig printed out with ":t f"!
-}
--f :: HasInitial c (TextF c) => (TextF c -> TextF c) -> String
f :: (HasInitial c b, Show c) => (TextF a -> b) -> [Char]

f :: (HasInitial v (TextF v, Text v)) => (TextF v -> TextF v) -> String
f d = show v where v = getInitial (d (Pars []))
-}

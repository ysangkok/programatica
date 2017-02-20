module PParsek where

import Monad( MonadPlus(..) )
import List ( union )
import Char

-- simplification of Koen Claessen's parsing combinators


-- ParsekType

newtype Parser s r a = Parser ((a -> Parse s r) -> Parse s r)

data Parse s r
  = Input (Parse s r) (s -> Parse s r)
  | Fail 
  | Result r (Parse s r)

instance Monad (Parser s r) where
  return a =
    Parser (\fut -> fut a)
  
  Parser f >>= k =
    Parser (\fut -> f (\a -> let Parser g = k a in g fut))

  fail s =
    Parser (\_ -> Fail)

instance MonadPlus (Parser s r) where
  mzero =
    Parser (\_ -> Fail)
    
  mplus (Parser f) (Parser g) =
    Parser (\fut -> f fut `plus` g fut)

plus :: Parse s res -> Parse s res -> Parse s res

Fail `plus` Fail = Fail

Input done1 sym1 `plus` Input done2 sym2 =
  Input (done1 `plus` done2) (\c -> sym1 c `plus` sym2 c)

p            `plus` Result res q = Result res (p `plus` q)
Result res p `plus` q            = Result res (p `plus` q)

p@(Input _ _) `plus` _             = p
_             `plus` q@(Input _ _) = q


-- satisfy :: (Eq s, Show s) => (s -> Bool) -> Parser s a s
satisfy pred =
  Parser (\fut ->
    let done          = Fail
        
        symbol c
          | pred c    = fut c
          | otherwise = Fail

     in Input done symbol
  )


string :: (Eq s, Show s) => [s] -> Parser s [s] [s]
string s =
  Parser (\fut ->
    let succ   = fut s
        fail   = Fail
    
        inputs []     = succ
        inputs (x:xs) =
          Input
            fail
              (\c -> if c == x then inputs xs
                               else fail)
     
     in inputs s
  )


-- ParsekCombinators

pzero :: Parser s r a
pzero = mzero

(<|>) :: Parser s r a -> Parser s r a -> Parser s r a
p <|> q = p `mplus` q

choice :: [Parser s r a] -> Parser s r a
choice ps           = foldr (<|>) mzero ps

option :: a -> Parser s r a -> Parser s r a
option x p          = p <|> return x

optional :: Parser s r a -> Parser s r ()
optional p          = do{ p; return ()} <|> return ()

many1,many :: Parser s r a -> Parser s r [a]
many1 p             = do{ x <- p; xs <- many p; return (x:xs) }

many p              = scan id
                    where
                      scan f    = do{ x <- p
                                    ; scan (\tail -> f (x:tail))
                                    }
                                <|> return (f [])



-- Parsek

parse :: Parser s a a -> ParseMethod s a r -> [s] -> r
parse (Parser f) method xs = method (f (\a -> Result a Fail)) xs

type ParseMethod s a result = Parse s a -> [s] -> result

longestResults :: ParseMethod s a [a]
longestResults p xs = scan p [] [] xs
 where
  scan (Input _ sym) []  old (x:xs) = scan (sym x) [] old xs

  scan (Input _ sym) new _   (x:xs) = scan (sym x) [] new xs

  scan (Input done _) new old []    = scan done new old []

  scan (Result res p) new old xs    = scan p    (res:new) [] xs

  scan Fail           []  []  _     = []
  scan Fail           []  old _     = old
  scan Fail           new _   _     = new

completeResults :: ParseMethod s a [(a,[s])]
completeResults p xs = scan p [] [] xs
 where
  scan (Input _ sym) []  old (x:xs) = scan (sym x) [] old xs

  scan (Input _ sym) new _   (x:xs) = scan (sym x) [] new xs

  scan (Input done _) new old []    = scan done new old []

  scan (Result res p) new old xs    = scan p    ((res,xs):new) [] xs

  scan Fail           _   _   (_:_) = []
  scan Fail           []  []  _     = []
  scan Fail           []  old _     = old
  scan Fail           new _   _     = new

-- to test

myparse = parse (do {ds <- many1 (satisfy isDigit) ; return 'c'}) completeResults

-- for GF

closure p v = (p v  >>= closure p) <|> return v

(*?*) :: Parser a b c -> (c -> Maybe d) -> Parser a b d
p *?* f = do
  x <- p 
  (case f x of 
     Just c -> return c
     _      -> pzero)

-- export to GF

useParsek :: Parser s a a -> [s] -> [(a,[s])]
useParsek p = parse p completeResults


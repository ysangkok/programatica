module HBCParsers(
  Parser,
  -- Invoking parsers:
  parseResults,newParseResults,

  -- std combinators:
  (***), -- Functor
  succeed,(.>.), -- Monad
  fails,(|||), -- MonadPlus
  (+||),
  item,--satisfy,literal,
  satisfyspan,
  -- left recursion:
  rule,
  -- suspicious, nonstandard combinators:
  --first,
  complete,remainingInput,(.<.),
  -- backward compatibility
  parses,wadlerParser
 ) where

--import Operations
import qualified Parse as P -- HBC parser combinators
import List(nub)
import ListUtil(mapSnd)
import LexPos
--import Trace(trace)

infixr 2 |||, +||
infixr 3 ***
infixr 5 .>.

-- An interface to the parser combinators from the library supplied with HBC.

newtype Parser a b = P (P.Parser (Input a) b)
unP (P p) = p

--type Input a = (Pos,[a]) -- if position is computed by the item combinator
--type Input a = [a] -- if positions are added to all tokens by a tokenizer
type Input a = (StopList,[a]) -- for left recursion

type StopList = [Int] -- forbidden rules
emptystoplist = []
extendstoplist = (:)

-- Higher level way to invoke a parser
--newParseResults :: Parser a b -> [a] -> Either String b
--newParseResults (P p) s= convResult (P.parse (complete' p) s)
newParseResults p s=
  convResult (P.parse (unP $ complete (p.<.const s)) (emptystoplist,[]))

convResult result =
  case result of
    Right ((x,(_,[])):_) -> Right x
    Right ((_,(_,rest)):_)   -> Left ("Syntax error after "++showPos rest)
    Left (expected,(_,rest)) -> Left ("Syntax error at "++showPos rest++": "++
				      --lexShow rest++"\n"++
				      "Expected: "++unwords (nub expected))

showPos [] = "end of input"
showPos (t:_) = show (lexPos t)

--parseResults :: Parser a b -> [a] -> [b]
parseResults = (map fst.) . parses . complete

-- For external use, only for backward compatibility:
parses :: Parser a b -> [a] -> [(b,[a])]
parses (P p) = either (const []) (mapSnd snd) . P.parse p . (,) emptystoplist

--wadlerParser :: ([a] -> [(b,[a])]) -> Parser a b
wadlerParser wp = P $ ptoken wadp
  where
    wadp s = case wp s of
	           [] -> Left "walder parser failed"
	           (x,r):_ -> Right (x,r)
				-- !! can only return one result

--- Standard combinators:

succeed :: b -> Parser a b
succeed = P . P.succeed

fails :: String -> Parser a b
fails = P . P.failure

(|||) :: Parser a b -> Parser a b -> Parser a b
P p1 ||| P p2 = P $ p1 P.||| p2

(+||) :: Parser a b -> Parser a b -> Parser a b
P p1 +|| P p2 = P $ p1 P.||! p2

(.>.) :: Parser a b -> (b -> Parser a c) -> Parser a c
P p1 .>. p2 = P $ p1 `P.into` unP . p2

--apP p2 x = case p2 x of P p -> p

(***) :: Parser a b -> (b -> c) -> Parser a c
P p *** f = P $ p P.>>- f
--p *** f = p .>. \ x -> let y =f x in seq y (succeed y)

--item :: Parser a a
item = P $ ptoken item'
  where
    item' (_,[]) = Left "end of input"
    item' (_,a:rest) = Right (a,(emptystoplist,rest))

--satisfy :: (a -> Bool) -> Parser a a
--satisfy = P . P.litp "<satisfy>" -- !!

--literal :: (Eq a,Show a) => a -> Parser a a
--literal = P . P.lit

satisfyspan tst = P $ ptoken span'
  where
    span' (stoplist,is) =
      case span tst is of
	(is1,is2) ->
	  if null is1
	  then Right (is1,(stoplist,is2))
	  else Right (is1,(emptystoplist,is2))
	  -- always succeeds?

--- Left recursion

rule n (P p) = P $ rule' n p
  where
    rule' n p = rule'' n P...+ p
    --rule' n p = rule'' n `P.into` \ old -> p P.+.. rule_end n old
    rule'' n = ptoken $ rule''' n
    rule''' n (stoplist,input) =
      if n `elem` stoplist
      then --trace ("prevent left recursion "++show (n,stoplist,input)) $
	   Left "left recursion"
      else --trace ("recording rule "++show (n,stoplist,input)) $
	   Right ((){-stoplist-},(extendstoplist n stoplist,input))

    {-
    rule_end n oldstoplist = ptoken $ rule_end' n oldstoplist
    rule_end' n oldstoplist (stoplist,input) =
	--trace ("end of rule "++show (n,oldstoplist,stoplist,stoplist',input)) $
	seq stoplist' $ Right ((),(stoplist',input))
      where
	stoplist' =
	  case stoplist of
	    n':rest | n'==n -> rest
	    [] -> []
	    _ -> trace ("surprising stoplist! "++show stoplist) stoplist
    -}

--- Nonstandard combinators:

--first :: Parser a b -> Parser a b
--first p = --trace "HBCParsers.first not implemented"
--	  p

complete (P p) = P $ complete' p 	-- suspicious as a combinator

complete' p = p P.+.. eof
  where
    eof = ptoken eof'
    eof' i@(_,[]) = Right ((),i)
    eof' _      = Left "end of input"

remainingInput = P $ ptoken r -- for dirty trick in XML parser
  where r i@(stoplist,s) = Right (s,i)

-- Input preprocessor:
--(.<.) :: Parser i a -> ([i] -> [i]) -> Parser i a
P p .<. f = {-trace ".<." $-} P $ ptoken pre P...+ p
  where pre (stoplist,s) = Right ((),(stoplist,f s))

ptoken f = P.token f
--  where f' x = trace (show $ either Left (Right . fst) $ y) y where y=f x

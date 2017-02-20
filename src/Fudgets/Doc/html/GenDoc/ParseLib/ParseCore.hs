module ParseCore(Pos,ParseError(..),ParseResult(..),ParseBad(..),ParseGood(..),Parser(..)
               ,initError,initBad,initGood      -- Start values for parseError,parseBad, parseGood
	       ,parseit				-- entry for parsing
               ,parse,bind,ap,chk,orelse        -- The core
               ,token                           -- parse terminal
	       ,eof                             -- parse end of file
               ,parseFail                       -- Failing parser
             --,maxError                        -- Keep "best" error message
               ) where

--import Prelude(error,(.),not,ord,chr,otherwise,(&&),(||),fromIntegral)
--import Prelude((++),show)
import Pos(Pos,noPos,eofPos)

infixl 5 `ap`
infixl 5 `chk`
infixr 4 `orelse`

--- Define types
type ParseError  p   i   = (p,Maybe i,[String])
type ParseResult p a i   = Either (ParseError p i) (a,[(p,i)],ParseError p i) 
type ParseBad    p a i   =                 ParseError p i -> ParseResult p a i 
type ParseGood   p a i c = a -> [(p,i)] -> ParseError p i -> ParseResult p c i
type Parser      p a i c = ParseGood p a i c -> ParseBad p c i -> [(p,i)] -> ParseError p i ->  ParseResult p c i

--- start values
--initError :: ParseError (Pos a) i
initError = (noPos,Nothing,["No error"])
eofError p = (p,Nothing,["End of file"])

initBad :: ParseBad p a i
initBad = \err -> Left err

p `addError` errmsg  = \ good bad -> p good (\(p,x,ss)->bad (p,x,errmsg:ss))

initGood :: ParseGood p a i a
initGood = \res input err -> Right (res,input,err)

parseit :: Parser (Pos p) a i a -> [(Pos p,i)] -> Either (ParseError (Pos p) i) a
parseit p input = parseit' (p initGood initBad input initError)

parseit' (Left err) = Left err
parseit' (Right (a,_,_)) = Right a -- no way to get the remaining input

--- The core
parse :: a -> Parser p a i c
parse x = \good bad -> good x

bind :: Parser p a i c -> (a->Parser p b i c) -> Parser p b i c
bind   x y = \good bad ->
                x       (\u -> y u good bad)
		        bad

ap :: Parser p (a->b) i c -> Parser p a i c -> Parser p b i c
ap     x y = \good bad ->
                x       (\u -> y (\v -> good (u v) ) bad)
                        bad

chk :: Parser p b i c -> Parser p a i c -> Parser p b i c
chk     x y = \good bad ->
                x       (\u -> y (\_ -> good u) bad)
                        bad

orelse :: Parser p a i b -> Parser p a i b -> Parser p a i b
x `orelse` y = \good bad input ->
        x good (y good bad input) input

token :: (Pos p -> i -> Either String a) -> Parser (Pos p) a i c
token p good bad []              err = bad (eofPos,Nothing,["A token (not eof)"])
token p good bad ((pos,t):input) err =
        case p pos t of
                Right tt' -> good tt' input err
                Left  f   -> bad (maxError (pos, Just t,[f]) err)

eof good bad [] err = good () [] err
eof good bad ((p,_):_)  err = bad (maxError (eofError p) err)

parseFail :: Parser p a i b
parseFail = \good bad input err -> bad err


maxError (a@(pa,ta,ma)) (b@(pb,tb,mb)) =
        if pa > pb then a
        else if pb > pa then b
             else (pa,ta,ma++mb)

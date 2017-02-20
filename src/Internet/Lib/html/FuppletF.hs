module FuppletF where
import AllFudgets -- because of autoLayoutF
--import Fudgets
import ParseURL(parseURL)
import URL(URL,joinURL)
import Html(Html)
import ListMap(lookupWithDefault)

type FuppletInput = (FuppletOutput,F () ())
type FuppletOutput = URL

fuppletF :: URL -> (Maybe String,[(String,String)],Html) -> F FuppletInput FuppletOutput
fuppletF purl (src,attrs,html) =
    maybe altF fupF (src >>= parseURL)
 where
   altF = sepF 3 $ labelF alt
   alt  = lookupWithDefault attrs "?fupplet?" "ALT"

   fupF rurl = sepF 3 $ putF (joinURL purl rurl) fuppletDisplayF >=^< snd

   fuppletDisplayF = getF $ autoLayoutF . stubF . ctrace "fupplet" src
   -- autoLayoutF isn't needed here because activeGraphicsF applies autoLayoutF
   -- to all active parts. However, to get the benefits of layoutDoNow, we
   -- still apply it here.

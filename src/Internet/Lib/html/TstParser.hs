import qualified HtmlParser as P1
import qualified HtmlParser2 as P2
import HtmlPrinter
import Html
import ParsOps(Error(..))
import System(getArgs)

main =
 do argv <- getArgs
    let parseHtml = if "1" `elem` argv then P1.parseHtml else P2.parseHtml
    interact ((++"\n").either show printHtml.parseHtml)
--    interact ((++"\n"). show .parseHtml)

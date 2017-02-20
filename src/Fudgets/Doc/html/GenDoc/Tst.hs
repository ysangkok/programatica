import Syntax
import Parser
import ParsOps
import RmComments
import ListUtil(chopList)

main = interact (show.interfacesParser)
--main = interact (show.chopList (head.lex).rmcomments)

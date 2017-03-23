module ParsedSyntax(module PosSyntax,Src,lexerPass0,Id(..)) where
import PosSyntax
import HsLexerPass1(lexerPass0)

type Src i = SN i

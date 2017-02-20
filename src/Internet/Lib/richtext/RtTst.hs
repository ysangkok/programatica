module Main(main) where
import AllFudgets renaming (Left to Ld,Right to Rd)
import RichText
import RichTextF
import RichTextParser
import IO(openFile)
main = fudlogue (simpleShellF "RichText" [] Nothing (richTextF Nothing)>==<rtFilePickF)

rtFilePickF = rt >^=< simpleShellF "File" [] Nothing smallFilePickF

rt filename =
     case openFile filename of
       Right s -> case (parseRichText.unlines.snd.break (""==).lines) s of
		    Right rt ->  rt
		    Left (Error s cs) -> errmsg (s++": "++cs)
       Left err -> errmsg err

errmsg s = [PlainChars s]

smallFilePickF =
  let lf (Layout _ fh fv) = Layout (P 160 250) fh fv
  in layoutModifierF lf filePickF

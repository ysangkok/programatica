module HtmlUtils where
--import ListUtil(concatMap)

escape = concatMap esc
  where esc c = case c of
                  '<' -> "&lt;"
		  '>' -> "&gt;"
		  '&' -> "&amp;"
		  _   -> [c]

wrap cmd txt = on cmd++txt++off cmd
tag cmd = "<"++cmd++">"
on = tag
off cmd = tag ('/':cmd)
html f = dq (f++".html")
sq s = '\'':s++"'"
dq s = '"':s++"\""

strong = wrap "STRONG"

imgsrc arg = on ("IMG SRC="++dq arg)



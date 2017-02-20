module HtmlConOps where
import Html
import HtmlTags

--- HTML construction: ---

html = ctx HTML . (nl:)
title = ctx TITLE
head' = ctx HEAD . (nl:)
body = ctx BODY . (nl:)
body' attrs = ctx' BODY attrs . (nl:)

br = cmd BR
p = ctxie P
p' = cmd P -- obsolete
hr = cmd HR

img src = cmd' IMG [("SRC",src)]
imgalt src alt = cmd' IMG [("SRC",src),("ALT",alt)]

h1 = ctx H1
h2 = ctx H2
h3 = ctx H3
h4 = ctx H4
h5 = ctx H5
h6 = ctx H6

pre = ctx PRE

ol = ctx OL
ul = ctx UL
menu = ctx MENU . map li
dir = ctx DIR . map li

li' = cmd LI -- obsolete
li = ctxie LI

dl = ctx DL
dlcompact = ctx' DL [("COMPACT","")]
dt = ctxie DT
dd = ctxie DD

form = form' []
form' attrs = HtmlContext (FORM, attrs)

input name attrs = cmd' INPUT (("name",name):attrs)
submit label = cmd' INPUT [("type","submit"),("value",label)]
reset label = cmd' INPUT [("type","reset"),("value",label)]

href url = ctx' A [("HREF",url)]
target name = ctx' A [("NAME",name)]

strong = ctx STRONG
em = ctx EM

tr = ctxie TR
td = ctxie TD

cmd s = cmd' s []
cmd' s attrs = HtmlCommand (s,attrs)
ctx s = ctx' s []
ctx' s attrs = HtmlContext (s,attrs)

ctxie s = ctxie' s []
ctxie' s attrs = ctx' s (("implicitend",""):attrs)

ctx_class s c = ctx' s [("CLASS",c)]
div_class = ctx_class DIV
span_class = ctx_class SPAN

txt = HtmlChars
nl = txt "\n" -- Can be used to improve readability of generated HTML

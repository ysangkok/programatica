module HtmlIsIndexForm where
import Html
import HtmlTags
import HtmlConOps(form,hr,p,cmd,txt)
import Data.ListMap(lookupWithDefault)

isIndexHtml attrs =
    form [ hr, p [txt prompt], p [input], hr ]
  where
    defPrompt = "This is a searchable index. Enter your search keywords below."
    prompt = lookupWithDefault attrs defPrompt "PROMPT"
    input = HtmlCommand (INPUT,[("NAME","")])

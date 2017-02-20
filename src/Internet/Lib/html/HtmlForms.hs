module HtmlForms(HtmlForm(..),HtmlInput(..),Attrs(..),Options(..),extractForms,htmlInputAttrs) where
import HtmlOps(htmlchars)
import Html
import HtmlTags

type HtmlForm = (Attrs,[HtmlInput]) -- (form attrs,form elements)

data HtmlInput = FormInput Attrs
               | FormSelect Attrs Options
	       | FormTextArea Attrs String
	       | FormIsIndex
	       deriving (Eq,Show)

htmlInputAttrs inp =
  case inp of
    FormInput a -> a
    FormSelect a _ -> a
    FormTextArea a _ -> a
    _ -> []

type Attrs = [(String,String)]
type Options = [(Bool,String)] -- (selected,value) -- used to be called Options..

extractForms :: Html -> [HtmlForm]
extractForms = concatMap extr
  where
    extr (HtmlContext ctx html) =
         case ctx of
	   (FORM,attrs) -> [(attrs,extractHtmlInput html)]
	   _ -> extractForms html
    extr (HtmlCommand (ISINDEX,attrs)) = [(attrs,[FormIsIndex])]
    extr _ = []

extractHtmlInput = concatMap extr
  where
    extr e =
      case e of
        HtmlCommand (INPUT,as) -> [FormInput as]
	HtmlCommand (ISINDEX,_) -> [FormIsIndex]
	HtmlContext (SELECT,as) html -> [FormSelect as (options html)]
	HtmlContext (TEXTAREA,as) html -> [FormTextArea as (htmlchars html)]
	HtmlContext _ html -> extractHtmlInput html
	_ -> []

options [] = []
options (HtmlContext (OPTION,attrs) ohtml:html) =
  ("SELECTED" `elem` map fst attrs,trim (htmlchars ohtml)):options html
options (_:html) = options html

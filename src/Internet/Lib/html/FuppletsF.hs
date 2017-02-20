module FuppletsF(fuppletsF) where
import InsetsF
import Html
import HtmlTags
import FuppletFetchF
import FuppletF

fuppletsF = insetsF fupplets fuppletFetchF fuppletF

fupplets = concatMap extr
  where
    extr e =
      case e of
        HtmlContext (FUPPLET,attrs) html -> [(lookup "SRC" attrs,attrs,html)]
	HtmlContext _ html -> fupplets html
	_ -> []

module Html(module Html,TagName) where
import HtmlTags

type Html = [HtmlItem]

data HtmlItem = HtmlChars String
	      | HtmlContext HtmlTag Html	-- <s>...</s> (elements)
	      | HtmlCommand HtmlTag	        -- <s> (empty elements)
	      | HtmlGarbage HtmlBadTag          -- saved for debugging output
	      deriving (Eq,Show)

type HtmlTag = (TagName, TagAttrs)
type HtmlBadTag = (String, TagAttrs)
--type TagName = String
type TagAttrs = [(String,String)]

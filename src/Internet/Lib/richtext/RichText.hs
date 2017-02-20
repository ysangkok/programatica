module RichText where

data RichTextItem a = PlainChars String
                    | Special a
		    | FmtChar Switch CharFmt
		    | FmtPara ParaFmt (RichText a)
		    | NewLine
		    | NewPage
		    --deriving (Eq{-,Text-})

type RichText a = [RichTextItem a]

data Switch = Off | On deriving (Eq{-,Text-})

data CharFmt = Bold | Italic
             | Fixed | Sans 
	     | Smaller | Bigger | Underline | Subscript | Superscript
	     | UsAscii | Iso8859 Int | Symbol
	     | PenColor String 
	     | Anchor Anchor
	     deriving (Eq,Show)

type Anchor = String

data ParaFmt = Center | FlushLeft | FlushRight |
	       Indent | IndentRight | Outdent | OutdentRight |
	       SamePage | Heading | Footing |
	       Excerpt | Paragraph | Signature |
	       Comment'
		deriving (Eq{-,Text-})

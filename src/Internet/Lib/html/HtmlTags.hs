module HtmlTags where
import Ix

data TagName
  -- Head tags
  = TITLE | ISINDEX | META | LINK | BASE | SCRIPT | STYLE
    -- ISINDEX is also a block tag
  | NEXTID -- non-standard
  -- Block tags
  | H1 | H2 | H3 | H4 | H5 | H6
  | UL | OL | DIR | MENU | DL | P | PRE | BLOCKQUOTE | DIV | CENTER
  | FORM | HR | TABLE | ADDRESS
  -- New HTML 4.0 block tags:
  | NOSCRIPT
  -- Logical tags
  | EM | STRONG | DFN | CODE | SAMP | KBD | VAR | CITE
  -- Physical tags
  | TT | I | B | U | STRIKE | BIG | SMALL | SUB | SUP
  -- Special inline tags
  | A | BASEFONT | IMG | APPLET | FONT | BR | MAP
  | NOBR -- Netscape extension?
  | BLINK
  -- HTML 4.0:
  | S | Q | ABBR | ACRONYM | SPAN
  --  !! These are actually allowed both as block and inline elems:
  | DEL | INS
  | IFRAME | OBJECT
  -- Other tags
  | LI | DT | DD
  | INPUT | SELECT | TEXTAREA | OPTION
  | CAPTION | TR | TH | TD
  | AREA
  | PARAM
  | HTML | HEAD | BODY
  -- Nonstandard tags:
  | FRAMESET | FRAME | NOFRAMES
  | PLAINTEXT | SANS | LISTING
  | FUPPLET -- WWWBrowser
  | MULTICOL -- Netscape
  deriving (Eq,Show,Ord,Enum,Bounded,Ix)

allTags = [minBound .. maxBound] :: [TagName]
blockTagRange = (H1,NOSCRIPT)

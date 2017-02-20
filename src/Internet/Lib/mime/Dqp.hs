module Dqp(decodeQuotedPrintable) where -- decode Content-Transfer-Encoding Qouted-Printable
import Utils2(ord,chr)

decodeQuotedPrintable = dqp

dqp  [] = []
dqp ('=':'\r':'\n':cs) = dqp cs
dqp ('=':'\n':cs) = dqp cs
dqp ('=':c1:c2:cs) | (ishex c1 && ishex c2) = hex c1 c2:dqp cs
dqp (c:cs) = c:dqp cs

hex c1 c2 =
  let hexdig c = if c<'A' then ord c-ord '0'
   		 else if c<'a' then ord c-(ord 'A'-10)
		 else ord c-(ord 'a'-10)
  in chr (16*hexdig c1 + hexdig c2)

ishex c = '0'<=c && c<='9' || 'A'<=c && c<='F' || 'a'<=c && c<='f'


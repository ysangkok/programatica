module Unicode where

-- Like HBC's Char.decodeUTF8, but with more graceful failure.

decodeUTF8 :: String -> String
decodeUTF8 "" = ""
decodeUTF8 (c:cs) | c < '\x80' = c : decodeUTF8 cs
decodeUTF8 (c:c':cs) | '\xc0' <= c  && c  <= '\xdf' && 
		      '\x80' <= c' && c' <= '\xbf' =
	toEnum ((fromEnum c `mod` 0x20) * 0x40 + fromEnum c' `mod` 0x40) : decodeUTF8 cs
decodeUTF8 (c:c':c'':cs) | '\xe0' <= c   && c   <= '\xef' && 
		          '\x80' <= c'  && c'  <= '\xbf' &&
		          '\x80' <= c'' && c'' <= '\xbf' =
	toEnum ((fromEnum c `mod` 0x10 * 0x1000) + (fromEnum c' `mod` 0x40) * 0x40 + fromEnum c'' `mod` 0x40) : decodeUTF8 cs
--decodeUTF8 _ = error "UniChar.decodeUTF8: bad data"
decodeUTF8 (_:cs) = decodeUTF8 cs

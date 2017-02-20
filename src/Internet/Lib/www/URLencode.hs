module URLencode where
import Utils2Janus(mix,chr,ord)

encodeQuery values = mix (map urlEnc values) "&"
  where
    urlEnc (name,value) = encode name ++ "=" ++ encode value

encode :: Maybe Char -> [Char]
encode = concatMap enc
  where
    special c = c<' ' || c>'~' || c `elem` "\"<>&=%/?#+:()[]{}~" -- more?
    enc ' ' = "+"
    enc c | special c = '%':hex (ord c)
          | otherwise = [c]

hex n = [hexdigit (n `div` 16),hexdigit (n `rem` 16)]
  where
    hexdigit n = if n<10
                 then chr(n+ord '0')
		 else chr(n+(ord 'A'-10))

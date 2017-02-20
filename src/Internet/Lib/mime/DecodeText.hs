module DecodeText where
import Dqp
import Base64
import Dew
import MimeMessage
import ListMap(lookupWithDefault)
import Utils2(strToLower)

decodeTextMsg msg@(MimeMsg hdrs body) =
    if getheader hdrs "Mime-Version" /= "" && istext (mimeContentType hdrs)
    then case lookup cte encodings of
           Just decode -> MimeMsg (changeType cte hdrs') (decode body)
           _ -> MimeMsg hdrs' body
    else MimeMsg hdrs' body -- not mime, or not plain text, leave encoded...
  where
    cte = strToLower (getheader hdrs cte_hdr)
    hdrs' = decodeHeaders hdrs


-- Old:
decodeText msg =
  let (hdrs,bodylines)=splitmsg msg
      body = unlines bodylines
      hdrs' = decodeHeaders hdrs
  in if getheader hdrs "Mime-Version" /= "" && istext (mimeContentType hdrs)
     then let cte = strToLower (getheader hdrs cte_hdr)
          in if cte `elem` map fst encodings
	     then hdrlines (changeType cte hdrs') ++ [""] ++
                  lines (decode cte body)
	     else hdrlines hdrs' ++ [""] ++ bodylines
     else hdrlines hdrs' ++ [""] ++
          bodylines -- not mime, or not plain text, leave encoded...

changeType cte =
  updateHdr (cte_hdr,if strToLower cte=="7bit" then cte else "8BIT") .
  renameHdr cte_hdr cte_was_hdr

cte_hdr = "Content-Transfer-Encoding"
cte_was_hdr = "X-"++cte_hdr++"-Was"

istext ct = strToLower ct `elem` ["text/plain","text/richtext","text/html"]

decode = lookupWithDefault encodings (\x->x)

encodings =
  [("quoted-printable",decodeQuotedPrintable),
   ("base64",decodeBase64)]

decodeHeaders = modHdr "subject" decodeEncodedWords
                . modHdr "from" decodeEncodedWords
                . modHdr "to" decodeEncodedWords
  				-- should decode some other hdrs too!!

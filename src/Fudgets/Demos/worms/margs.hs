module margs(wargs) where
import Keys


keys (ks @ [l,r]) = zip (take 4 stdkeys ++ ks) (stdkeys)
keys (ks @ [u,l,d,r]) = zip (ks ++ drop 4 stdkeys)(stdkeys)
keys [] = zip (stdkeys)(stdkeys)
keys _ = zip (stdkeys)(stdkeys)	-- should generate error msg

wargs argv default_host =
  case argv
  of []->  Right (default_host,keys [])
     ['+':'k':k]-> Right (default_host,keys k)
     [h]-> Right (h,keys [])
     ["+k",k]-> Right (default_host,keys k)
     ['+':'k':k,h]-> Right (h,keys k)
     ["+k",k,h]-> Right (h,keys k)
     _-> Left("Usage: masken [ +k keys ] [ host ]\n" ++
	      "       host: name of machine where maskend is running\n" ++
	      "       keys: redefine control keys, e:g::\n" ++
	      "	         +k nm      -- n:turns left, m:turns right\n" ++
	      "             +k khlj    -- k:up h:left l:right j:down\n")

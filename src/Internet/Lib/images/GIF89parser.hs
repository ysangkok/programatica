module GIF89parser where
import GIF
import Utils2(bit,bits)

-- Interpret some of the extension blocks defined in GIF89a

interpretGIF89a gif = gif { data_blocks=map db (data_blocks gif) }
  where
    db = either (Left . gif89eb) Right

    gif89eb eb =
      case eb of
	EB 254 fd -> Comment (map toEnum (concat fd))
	EB 255 (b1:bs) | length b1==11 -> AE (map toEnum ai) (ac1,ac2,ac3) bs
	  where (ai,[ac1,ac2,ac3]) = splitAt 8 b1
	EB 249 [[b1,b2,b3,b4]] -> GCE disp ui istransp delay transp
	  where
	    disp = bits 2 3 b1
	    ui = bit 1 b1
	    istransp = bit 0 b1
	    delay = b1+256*b2
	    transp = b4
	_ -> eb

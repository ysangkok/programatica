module Main(main) where
import Fudgets

font = if null args
       then "-adobe-times-bold-r-normal--34-*-*-*-*-*-*-*"
       else head args

--font = "-adobe-times-bold-r-normal--34-*-*-*-*-*-*-*"

myUntaggedListLF layout fs = snd >^=< listLF layout (number 0 (concat fs))

startstate = ([0::Integer], 10, False)

digit d = ["0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ"!!(fromInteger d)]
bconv b x = if x < b then digit x else bconv b (x`div`b) ++ digit (x`mod`b)
showstate (x:xs, b, _) = (if x < 0 then '-':bconv b (0-x) else bconv b x)++"    "++show b
bop f ([x], b, c) = bop f ([x,x], b, True)
bop f (x:y:xs, b, c) = (f y x:xs, b, True)
uop f (x:xs, b, c) = (f x:xs, b, True)
dig n (x:xs, b, c) = ((if c then n:x:xs else if n < b then x*b+n:xs else x:xs), b, False)

accumulator =
  let transform state op = let state' = op state
			   in (state',state')
  in putSP startstate $ mapAccumlSP transform startstate

disp = displayF' (setAlign aRight.setFont font)  >=^< showstate

buttons =
  let b = buttonF
  in let bf s f = (\x->f) >^=< b s
  in let bq s = quitF>==<b s

  in let buttons = [
	   [bq "Off", bf "Enter" (\(x:xs, b, c)->(x : x : xs, b, True)), bf "Clear" (\(_:xs, b, c)->(0:xs, b, False)), bf "BS" (\(x:xs, b, c)->((if c then x else x`div`b):xs, b, c))],
	   [bf "D" (dig 13),bf "E" (dig 14),bf "F" (dig 15),bf "mod" (bop (mod))],
	   [bf "A" (dig 10),bf "B" (dig 11),bf "C" (dig 12),bf "/" (bop (div))],
	   [bf "7" (dig 7), bf "8" (dig 8), bf "9" (dig 9), bf "*" (bop (*))],
	   [bf "4" (dig 4), bf "5" (dig 5), bf "6" (dig 6), bf "-" (bop (-))],
	   [bf "1" (dig 1), bf "2" (dig 2), bf "3" (dig 3), bf "+" (bop (+))],
	   [bf "0" (dig 0), bf "Chs" (uop (0-)), bf "Base" (\(x:xs, b, c)->(x:xs, (if x <= 36 && x > 1 then x else b), True)), bf "Pop" (\(x:xs, b, c)->(if null xs then ([x],b,c) else (xs,b,c))) ]]

  in let layout = matrixP 4
  in myUntaggedListLF layout buttons

counterF = (disp >=^^< accumulator, Above) >#==< buttons

main = fudlogue $ shellF "Calculator" counterF

--[] ++ ys = ys
--(x:xs) ++ ys = x:(xs++ys)

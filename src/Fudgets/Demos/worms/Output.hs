module Output where
import IOlml(doFlush)
import io
import Termcap

data OUTPUT = Str' String | Flush | Clr | Moveto Int Int

print' (Str' s)= s
print' Flush =  doFlush
print' Clr = clear
print' (Moveto x y) = moveTo x y

prs = pr . (:[]) . Str'

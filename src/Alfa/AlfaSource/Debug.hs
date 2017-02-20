module Debug2 where
import Fudgets
import qualified NonStdTrace as Tr

trace = if argFlag "trace" False
        then Tr.trace
	else const id

badtrace = Tr.trace

tr x = trace (show x) x

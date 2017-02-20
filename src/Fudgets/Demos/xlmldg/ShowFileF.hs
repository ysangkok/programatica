module ShowFileF(showfileF) where
import Fudgets
import env(srcExt)

showfileF = showF>=^^<concatMapSP extrfile

showF = if outputNames
        then echoF
	else moreFileShellF

echoF = stdoutF >=^< (++"\n")

extrfile (Left s) = if outputNames then [s] else []
extrfile (Right (_,s)) = [s ++ srcExt]

outputNames = argKey "outputnames" "no" == "yes"

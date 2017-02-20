module Main where
import Fudgets
import ContribFudgets (filePickF)

main = fudlogue
         (moreFileShellF>==<
	  shellF "File Dialogue" (
	    filterJustSP >^^=< startupF [Nothing] filePickF))

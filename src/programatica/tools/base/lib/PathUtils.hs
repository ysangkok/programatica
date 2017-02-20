-- Functions for manipulating FilePaths
module PathUtils where

-- Normalize file names:
normf ('.':'/':s) = normf s
normf s = s

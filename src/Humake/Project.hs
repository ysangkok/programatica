module Project where
import ModuleIds
import FileNames

data Project = P {targets::[(ModuleName,FileType)],
	          srcdirs,libdirs::[String],
		  objdir::PFilePath,
		  ignoreimports::[ModuleName]}
		deriving (Show)

project0 = P

roots = map fst.targets

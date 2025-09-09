module Data.LLVM.Builder.Debug
  
public export 
record FileInfo where 
    constructor MkFileInfo
    fileName : String
    directory : String

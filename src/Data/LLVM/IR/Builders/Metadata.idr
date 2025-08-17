module Data.LLVM.IR.Builders.Metadata

import Data.LLVM.IR.Builders.Core
import Data.LLVM.IR.Builders.Ops
import Data.LLVM.IR.Core
import Data.LLVM.IR.Ops
import Data.LLVM.IR.Builders.Helper

-- DICompileUnit TODO:
-- FIXME: Check to see if theres a better way to handle this
inFile : String -> String -> Metadata 
inFile name dir = MetadataSpecial "DIFile" [("filename", name), ("directory", dir)]

-- DI types
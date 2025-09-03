module Data.LLVM.Ops.Metadata

import Data.LLVM.Ops.Core
import Data.LLVM.Ops.Ops
import Data.LLVM.IR.Core
import Data.LLVM.IR.Core
import Data.LLVM.Ops.Helper

-- DICompileUnit TODO:
-- FIXME: Check to see if theres a better way to handle this
inFile : String -> String -> Metadata 
inFile name dir = MetadataSpecial "DIFile" [("filename", name), ("directory", dir)]

-- DI types
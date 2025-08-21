module Data.LLVM.Builders.Metadata

import Data.LLVM.Builders.Core
import Data.LLVM.Builders.Ops
import Data.LLVM.IR.Core
import Data.LLVM.IR.Ops
import Data.LLVM.Builders.Helper

-- DICompileUnit TODO:
-- FIXME: Check to see if theres a better way to handle this
inFile : String -> String -> Metadata 
inFile name dir = MetadataSpecial "DIFile" [("filename", name), ("directory", dir)]

-- DI types
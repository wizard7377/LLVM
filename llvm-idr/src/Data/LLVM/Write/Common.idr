module Data.LLVM.Write.Common

import Data.LLVM.Write.Types
import Data.LLVM.IR

attributeNameKind : Attribute -> String
attributeNameKind ZeroExt = "zeroext"
attributeNameKind SignExt = "signext"
attributeNameKind _ = ?_
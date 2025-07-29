module Test.Groups.Encoding

import Data.LLVM
import Data.LLVM.IR
import Data.LLVM.Write
import Data.LLVM.Class
import Test.Helper
public export
encodingTests : IO ()
encodingTests = do
    putStrLn "=== Running LLVM Encoding Tests ==="
    
    -- Test basic types
    debugTest "Integer Type 32" (LType.LInt 32)
    debugTest "Integer Type 64" (LType.LInt 64)
    debugTest "Void Type" LType.LVoid
    debugTest "Pointer Type" LType.LPtr
    debugTest "Float Type" (LType.LFloating LType.LFloat)
    debugTest "Double Type" (LType.LFloating LType.LDouble)
    
    -- Test aggregate types
    debugTest "Array Type" (LType.LArray 10 (LType.LInt 32))
    debugTest "Struct Type" (LType.LStruct [LType.LInt 32, LType.LInt 64, LType.LPtr])
    debugTest "Packed Struct Type" (LType.LPackedStruct [LType.LInt 8, LType.LInt 16])
    debugTest "Vector Type" (LType.LVector 4 (LType.LFloating LType.LFloat))
    
    -- Test function types
    debugTest "Function Type" (LType.LFun (LType.LInt 32) [LType.LInt 32, LType.LPtr])
    debugTest "VarArg Function Type" (LType.LFunVarArg (LType.LVoid) [LType.LInt 32] (LType.LInt 32))
    
    -- Test linkage types
    debugTest "Private Linkage" Private
    debugTest "Internal Linkage" Internal
    debugTest "External Linkage" External
    debugTest "LinkOnce Linkage" LinkOnce
    

    -- Test calling conventions
    debugTest "C Calling Convention" C
    debugTest "Fast Calling Convention" Fast
    debugTest "Custom Calling Convention" (CustomCC 42)
    
    -- Test visibility
    debugTest "Default Visibility" Default
    debugTest "Hidden Visibility" Hidden
    debugTest "Protected Visibility" Protected
    
    -- Test names
    debugTest "Local Name" (Local "myvar")
    debugTest "Global Name" (Global "globalvar")
    debugTest "Special Name" (Special "specialvar")
    debugTest "Metadata Name" (MetadataN "metadata1")
    
    -- Test constants
    debugTest "Integer Constant" (Core.LInt 42)
    debugTest "Float Constant" (LFloat "3.14159")
    debugTest "Bool Constant True" (LBool True)
    debugTest "Bool Constant False" (LBool False)
    debugTest "Null Constant" LNull
    debugTest "String Constant" (LString "Hello, World!")
    debugTest "Undefined Constant" LUndefined
    debugTest "Poison Constant" LPoison
    debugTest "Zero Constant" LZero
    
    -- Test expressions
    debugTest "Constant Expression" (LConstE (LInt 100))
    
    -- Test attributes
    debugTest "ZeroExt Attribute" ZeroExt
    debugTest "SignExt Attribute" SignExt
    debugTest "Align Attribute" (Align 8)
    debugTest "ByVal Attribute" (ByVal (LType.LInt 32))
    debugTest "NoAlias Attribute" NoAlias
    
    -- Test address spaces
    debugTest "Named Address Space" (NamedSpace "cuda")
    debugTest "Unnamed Address Space" (UnnamedSpace 1)
    
    -- Test DLL storage
    debugTest "DLL Export" DLLExport
    debugTest "DLL Import" DLLImport
    
    -- Test thread locality
    debugTest "Local Dynamic" LocalDynamic
    debugTest "Initial Exec" InitialExec
    
    -- Test preemption
    debugTest "Preemptible" Preemptible
    debugTest "Non-Preemptible" NonPreemptible
    
    -- Test address info
    debugTest "Unnamed Global" UnnamedGlobal
    debugTest "Unnamed Local" UnnamedLocal
    
    -- Test binary opcodes
    debugTest "Add Opcode" Add
    debugTest "Sub Opcode" Sub
    debugTest "Mul Opcode" Mul
    debugTest "And Opcode" And
    debugTest "Or Opcode" Or
    debugTest "Xor Opcode" Xor
    
    -- Test unary opcodes
    debugTest "FNeg Opcode" FNeg
    
    -- Test wrapping
    debugTest "No Signed Wrap" NoSigned
    debugTest "No Unsigned Wrap" NoUnsigned
    debugTest "No Signed/Unsigned Wrap" NoSignedUnsigned
    
    putStrLn "=== All Encoding Tests Completed ==="
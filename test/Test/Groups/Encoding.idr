module Test.Groups.Encoding

import Data.LLVM
import Data.LLVM.IR
import Data.LLVM.Write.Assembly
import Data.LLVM.Write.Foreign
import Data.LLVM.Class
import Test.Helper
public export
encodingTests : IO ()
encodingTests = do
    putStrLn "=== Running LLVM Encoding Tests ==="
    
    -- Test basic types
    encodeFCMTest "Integer Type 32" (LType.LInt 32)
    encodeFCMTest "Integer Type 64" (LType.LInt 64)
    encodeFCMTest "Void Type" LType.LVoid
    encodeFCMTest "Pointer Type" LType.LPtr
    encodeFCMTest "Float Type" (LType.LFloating LType.LFloat)
    encodeFCMTest "Double Type" (LType.LFloating LType.LDouble)
    
    -- Test aggregate types
    encodeFCMTest "Array Type" (LType.LArray 10 (LType.LInt 32))
    encodeFCMTest "Struct Type" (LType.LStruct [LType.LInt 32, LType.LInt 64, LType.LPtr])
    encodeFCMTest "Packed Struct Type" (LType.LPackedStruct [LType.LInt 8, LType.LInt 16])
    encodeFCMTest "Vector Type" (LType.LVector 4 (LType.LFloating LType.LFloat))
    
    -- Test function types
    encodeFCMTest "Function Type" (LType.LFun (LType.LInt 32) [LType.LInt 32, LType.LPtr])
    encodeFCMTest "VarArg Function Type" (LType.LFunVarArg (LType.LVoid) [LType.LInt 32] (LType.LInt 32))
    
    -- Test linkage types
    encodeFCMTest {b = CEnum} "Private Linkage" Private
    encodeFCMTest {b = CEnum} "Internal Linkage" Internal
    encodeFCMTest {b = CEnum} "External Linkage" External
    encodeFCMTest {b = CEnum} "LinkOnce Linkage" LinkOnce
    

    -- Test calling conventions
    encodeFCMTest {b = CEnum} "C Calling Convention" C
    encodeFCMTest {b = CEnum} "Fast Calling Convention" Fast
    encodeFCMTest {b = CEnum} "Custom Calling Convention" (CustomCC 42)
    
    -- Test visibility
    encodeFCMTest {b = CEnum} "Default Visibility" Default
    encodeFCMTest {b = CEnum} "Hidden Visibility" Hidden
    encodeFCMTest {b = CEnum} "Protected Visibility" Protected
    
    -- Test names
    -- encodeFCMTest "Local Name" (Local (id "myvar"))
    -- encodeFCMTest "Global Name" (Global "globalvar")
    -- encodeFCMTest "Special Name" (Special "specialvar")
    -- encodeFCMTest "Metadata Name" (MetadataN "metadata1")
    -- Test constants
    encodeFCMTest "Integer Constant" (withType (LType.LInt 8) $ LTerm.LInt 42)
    -- encodeFCMTest "Float Constant" (withType (LType.LFloating $ LFloat "3.14159"))
    encodeFCMTest "Bool Constant True" (withType (LType.LInt 1) $ LBool True)
    encodeFCMTest "Bool Constant False" (withType (LType.LInt 1) $ LBool False)
    encodeFCMTest "Null Constant" (withType LType.LPtr $ LNull)
    --encodeFCMTest "String Constant" (withType LType.L LString "Hello, World!")
    --encodeFCMTest "Undefined Constant" LUndefined
    --encodeFCMTest "Poison Constant" LPoison
    --encodeFCMTest "Zero Constant" LZero
    
    -- Test expressions
    --encodeFCMTest "Constant Expression" (LConstE (LInt 100))
    
    -- Test attributes
    --encodeFCMTest "ZeroExt Attribute" ZeroExt
    --encodeFCMTest "SignExt Attribute" SignExt
    --encodeFCMTest "Align Attribute" (Align 8)
    --encodeFCMTest "ByVal Attribute" (ByVal (LType.LInt 32))
    --encodeFCMTest "NoAlias Attribute" NoAlias
    {- 
    -- Test address spaces
    encodeFCMTest "Named Address Space" (NamedSpace "cuda")
    encodeFCMTest "Unnamed Address Space" (UnnamedSpace 1)
    
    -- Test DLL storage
    encodeFCMTest "DLL Export" DLLExport
    encodeFCMTest "DLL Import" DLLImport
    
    -- Test thread locality
    encodeFCMTest "Local Dynamic" LocalDynamic
    encodeFCMTest "Initial Exec" InitialExec
    
    -- Test preemption
    encodeFCMTest "Preemptible" Preemptible
    encodeFCMTest "Non-Preemptible" NonPreemptible
    
    -- Test address info
    encodeFCMTest "Unnamed Global" UnnamedGlobal
    encodeFCMTest "Unnamed Local" UnnamedLocal
    
    -- Test binary opcodes
    encodeFCMTest "Add Opcode" Add
    encodeFCMTest "Sub Opcode" Sub
    encodeFCMTest "Mul Opcode" Mul
    encodeFCMTest "And Opcode" And
    encodeFCMTest "Or Opcode" Or
    encodeFCMTest "Xor Opcode" Xor
    
    -- Test unary opcodes
    encodeFCMTest "FNeg Opcode" FNeg
    
    -- Test wrapping
    encodeFCMTest "No Signed Wrap" NoSigned
    encodeFCMTest "No Unsigned Wrap" NoUnsigned
    encodeFCMTest "No Signed/Unsigned Wrap" NoSignedUnsigned
    -}
    -- TODO:
    putStrLn "=== All Encoding Tests Completed ==="

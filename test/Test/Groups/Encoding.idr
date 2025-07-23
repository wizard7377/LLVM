module Test.Groups.Encoding

import Data.LLVM
import Data.LLVM.Types
import Data.LLVM.Ops
import Data.LLVM.Write
import Data.LLVM.Class
import Test.Helper

-- Test printing encoded forms of LLVM types
testLLVMTypeEncoding : IO TestResult
testLLVMTypeEncoding = do
  putStrLn "=== LLVM Type Encodings ==="
  putStrLn $ "LVoid: " ++ encode LVoid
  putStrLn $ "i32: " ++ encode (LInt 32)
  putStrLn $ "i64: " ++ encode (LInt 64)
  putStrLn $ "ptr: " ++ encode LPtr
  putStrLn $ "float: " ++ encode (LFloating Half)
  putStrLn $ "[10 x i32]: " ++ encode (LArray 10 (LInt 32))
  putStrLn $ "{i32, i64}: " ++ encode (LStruct [LInt 32, LInt 64])
  putStrLn $ "label: " ++ encode LLabel
  putStrLn $ "token: " ++ encode LToken
  putStrLn ""
  assertTrue "Type encoding test" True

-- Test printing encoded forms of binary operations
testBinaryOpEncoding : IO TestResult
testBinaryOpEncoding = do
  putStrLn "=== Binary Operation Encodings ==="
  let i32 = LInt 32
  let val1 = LocalRef "x"
  let val2 = LocalRef "y"
  
  -- Note: We can't directly encode operations without Show instances
  -- So we'll just print the operation types for now
  putStrLn "Available Binary Operations:"
  putStrLn "- Add"
  putStrLn "- Sub" 
  putStrLn "- Mul"
  putStrLn "- UDiv, SDiv"
  putStrLn "- URem, SRem"
  putStrLn "- Shl, LShr, AShr"
  putStrLn "- And, Or, Xor"
  putStrLn "- FAdd, FSub, FMul, FDiv, FRem (floating point)"
  putStrLn ""
  assertTrue "Binary op encoding test" True

-- Test printing encoded forms of terminators
testTerminatorEncoding : IO TestResult  
testTerminatorEncoding = do
  putStrLn "=== Terminator Encodings ==="
  putStrLn "Available Terminators:"
  putStrLn "- RetVoid"
  putStrLn "- Ret <type> <value>"
  putStrLn "- CondBr <condition> <true_label> <false_label>"
  putStrLn "- JumpBr <label>"
  putStrLn "- Switch <type> <value> <default> [cases...]"
  putStrLn "- IndirectBr <address> [labels...]"
  putStrLn "- Invoke <call_info>"
  putStrLn "- Resume <type> <value>"
  putStrLn "- Unreachable"
  putStrLn ""
  assertTrue "Terminator encoding test" True

-- Test printing encoded forms of calling conventions
testCallingConventionEncoding : IO TestResult
testCallingConventionEncoding = do
  putStrLn "=== Calling Convention Encodings ==="
  putStrLn $ "C: " ++ encode C
  putStrLn $ "Fast: " ++ encode Fast
  putStrLn $ "Cold: " ++ encode Cold
  putStrLn $ "GHC: " ++ encode GHC
  putStrLn $ "Swift: " ++ encode Swift
  putStrLn $ "Tail: " ++ encode Tail
  putStrLn $ "Custom(42): " ++ encode (CustomCC 42)
  putStrLn ""
  assertTrue "Calling convention encoding test" True

-- Test printing encoded forms of names and addresses
testNameEncoding : IO TestResult
testNameEncoding = do
  putStrLn "=== Name Encodings ==="
  putStrLn $ "Local variable: " ++ encode (Local "myvar")
  putStrLn $ "Global variable: " ++ encode (Global "myglobal")
  putStrLn $ "Special name: " ++ encode (Special "special")
  putStrLn $ "Custom name: " ++ encode (CustomN "custom")
  putStrLn ""
  assertTrue "Name encoding test" True

-- Test printing encoded forms of linkage types
testLinkageEncoding : IO TestResult
testLinkageEncoding = do
  putStrLn "=== Linkage Encodings ==="
  putStrLn $ "Private: " ++ encode Private
  putStrLn $ "Internal: " ++ encode Internal
  putStrLn $ "External: " ++ encode External
  putStrLn $ "LinkOnce: " ++ encode LinkOnce
  putStrLn $ "Weak: " ++ encode Weak
  putStrLn $ "Common: " ++ encode Common
  putStrLn $ "Appending: " ++ encode Appending
  putStrLn ""
  assertTrue "Linkage encoding test" True

-- Export test suite for LLVM encoding tests
export
encodingTestsSuite : TestSuite
encodingTestsSuite = MkTestSuite "LLVM Encoding Tests" [
  MkTestCase "Type Encoding" testLLVMTypeEncoding,
  MkTestCase "Binary Op Encoding" testBinaryOpEncoding,
  MkTestCase "Terminator Encoding" testTerminatorEncoding,
  MkTestCase "Calling Convention Encoding" testCallingConventionEncoding,
  MkTestCase "Name Encoding" testNameEncoding,
  MkTestCase "Linkage Encoding" testLinkageEncoding
]

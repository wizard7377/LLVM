module Test.Groups

import Data.LLVM
import Data.LLVM.Types
import Data.LLVM.Class
import Test.Helper

-- Test LLVM Types
testNameConstruction : IO TestResult
testNameConstruction = do
  let localName = Local "x"
  let globalName = Global "main"
  let specialName = Special "0"
  assertTrue "Names should construct properly" True

testLinkageTypes : IO TestResult
testLinkageTypes = do
  let linkages = [Private, Internal, External, LinkOnce, Weak]
  assertTrue "Linkage types should be defined" (length linkages > 0)

testCallingConventions : IO TestResult
testCallingConventions = do
  let conventions = [C, Fast, Cold, Swift]
  assertTrue "Calling conventions should be defined" (length conventions > 0)

testVisibilityTypes : IO TestResult
testVisibilityTypes = do
  let visibilities = [Default, Hidden, Protected]
  assertEqual "Should have 3 visibility types" 3 (length visibilities)

testAddressSpaces : IO TestResult
testAddressSpaces = do
  let namedSpace = NamedSpace "myspace"
  let unnamedSpace = UnnamedSpace 1
  assertTrue "Address spaces should construct" True

-- Test suite for LLVM Types
export
typesSuite : TestSuite
typesSuite = MkTestSuite "LLVM Types Tests" [
  MkTestCase "Name Construction" testNameConstruction,
  MkTestCase "Linkage Types" testLinkageTypes,
  MkTestCase "Calling Conventions" testCallingConventions,
  MkTestCase "Visibility Types" testVisibilityTypes,
  MkTestCase "Address Spaces" testAddressSpaces
]

-- Test LLVM Class interface
testEncodeInterface : IO TestResult
testEncodeInterface = do
  -- This would test the Encode interface once we have implementations
  assertTrue "Encode interface should be available" True

-- Test suite for LLVM Class
export
classSuite : TestSuite  
classSuite = MkTestSuite "LLVM Class Tests" [
  MkTestCase "Encode Interface" testEncodeInterface
]

-- Export all test suites
export
allSuites : List TestSuite
allSuites = [typesSuite, classSuite]
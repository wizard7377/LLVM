module Test.Groups

import Data.LLVM
import Data.LLVM.Types
import Data.LLVM.Class
import Test.Helper
import Test.Groups.Encoding
public export
allTests : IO ()
allTests = do
    putStrLn "Starting all tests..."
    encodingTests
    putStrLn "All tests completed."
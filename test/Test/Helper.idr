module Test.Helper

import Data.List
import Data.String

public export
data TestResult = Pass | Fail String

public export
Show TestResult where
  show Pass = "PASS"
  show (Fail msg) = "FAIL: " ++ msg

public export
record TestCase where
  constructor MkTestCase
  name : String
  test : IO TestResult

public export
record TestSuite where
  constructor MkTestSuite
  name : String
  tests : List TestCase

-- Helper function to assert equality
public export
assertEqual : (Show a, Eq a) => String -> a -> a -> IO TestResult
assertEqual msg expected actual = 
  if expected == actual
    then pure Pass
    else pure $ Fail $ msg ++ ": expected " ++ show expected ++ ", got " ++ show actual

-- Helper function to assert true
public export
assertTrue : String -> Bool -> IO TestResult
assertTrue msg True = pure Pass
assertTrue msg False = pure $ Fail msg

-- Helper function to assert false
public export
assertFalse : String -> Bool -> IO TestResult
assertFalse msg False = pure Pass
assertFalse msg True = pure $ Fail msg

-- Run a single test case
public export
runTest : TestCase -> IO Bool
runTest (MkTestCase name testFn) = do
  putStr $ "  " ++ name ++ "... "
  result <- testFn
  case result of
    Pass => do
      putStrLn "PASS"
      pure True
    Fail msg => do
      putStrLn $ "FAIL: " ++ msg
      pure False

-- Run a test suite
public export
runSuite : TestSuite -> IO (Nat, Nat) -- (passed, total)
runSuite (MkTestSuite name tests) = do 
    _ <- putStrLn $ "\n=== " ++ name ++ " ===" ;
    results <- traverse runTest tests ;
    putStrLn $ "\n" ++ show (length (filter id results)) ++ "/" ++ show (length results)  ++ " tests passed" ;
    pure (passed, total)
    
-- Run multiple test suites
public export
runAllSuites : List TestSuite -> IO ()
runAllSuites suites = do
    putStrLn "Running LLVM Tests..."
    results <- traverse runSuite suites
    let (totalPassed, totalTests) = foldl (\(p1, t1) : (_, _), (p2, t2) : (_, _) => (p1 + p2, t1 + t2)) (0, 0) results
    putStrLn $ "\n=== SUMMARY ==="
    putStrLn $ show totalPassed ++ "/" ++ show totalTests ++ " total tests passed"
    if totalPassed == totalTests
        then putStrLn "All tests passed! ✓"
        else putStrLn $ show totalPassed ++ " tests passed! (out of: " ++ show totalTests ++ ") ✗"
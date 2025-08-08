module Test.Groups.ModuleSetB

import Data.LLVM
import Data.LLVM.IR
import Data.LLVM.Write.Assembly
import Data.LLVM.Class
import Data.LLVM.IR.Builders.Math
import Test.Helper
import Data.LLVM.Write.Foreign


%hide Data.LLVM.IR.Builders.Core.emptyModule
-- Helper functions to create test modules
-- Module with function attributes and parameter attributes
{---- 
export
moduleWithAttributes : LModule
moduleWithAttributes = MkLModule {
    dataLayout = Nothing,
    target = Nothing,
    text = [
        FunctionDefC $ MkFunctionDef {
            name = "attributed_function",
            symbolInfo = MkSymbolInfo (Just External) Nothing (Just Default) Nothing,
            callingConvention = Just C,
            returnAttrs = [NoAlias],  -- Return attributes
            returnType = LPtr,
            args = [
                MkFunctionArgSpec LPtr [NoAlias] (Just "input"),   -- No alias
                MkFunctionArgSpec (LInt 32) [ZeroExt] (Just "size"),               -- Zero extend
                MkFunctionArgSpec LPtr [NoAlias] (Just "output")                -- No alias
            ],
            addressInfo = Nothing,
            addressSpace = Nothing,
            fnAttributes = [],  -- Function attributes
            section = Just ".text.hot",
            partition = Nothing,
            comdat = Nothing,
            alignment = Just 16,
            gc = Nothing,
            fprefix = Nothing,
            prologue = Nothing,
            personality = Nothing,
            metadata = [],
            body = [MkBlock "entry" [
                -- Memory copy with attributes
                Operation Discard (MiscOp (FnCallOp (MkFnCall NoTail [] (Just C) [] Nothing 
                    (LFun LVoid [LPtr, LPtr, LInt 32]) 
                    ( (LPtr (Global "llvm.memcpy.p0i8.p0i8.i32"))) 
                    [
                        MkWithType LPtr ( (LPtr (Local (NamedRegister "output")))),
                        MkWithType LPtr ( (LPtr (Local (NamedRegister "input")))),
                        MkWithType (LInt 32) ( (LPtr (Local (NamedRegister "size"))))
                    ] []))),
                Operation Discard (TerminatorOp (Ret LPtr ( (LPtr (Local (NamedRegister "output"))))))
            ] RetVoid],
            tags = []
        }
    ],
    tags = Nothing
}
export
-- Module with edge cases and error conditions
moduleWithEdgeCases : LModule
moduleWithEdgeCases = MkLModule {
    dataLayout = Nothing,
    target = Nothing,
    text = [
        -- Zero-sized array
        GlobalDefC $ MkGVarDef {
            name = "zero_array",
            symbolInfo = MkSymbolInfo (Just External) Nothing (Just Default) Nothing,
            threadLocality = Nothing,
            addressInfo = Nothing,
            addressSpace = Nothing,
            externallyInitialized = Nothing,
            isConst = True,
            gtpe = LArray 0 (LInt 8),
            initializer = Just LZero,
            tags = []
        },
        -- Function with unusual control flow
        FunctionDefC $ MkFunctionDef {
            name = "edge_case_function",
            symbolInfo = MkSymbolInfo (Just External) Nothing (Just Default) Nothing,
            callingConvention = Just C,
            returnAttrs = [],
            returnType = LInt 32,
            args = [MkFunctionArgSpec (LInt 32) [] (Just "input")],
            addressInfo = Nothing,
            addressSpace = Nothing,
            fnAttributes = [],
            section = Nothing,
            partition = Nothing,
            comdat = Nothing,
            alignment = Nothing,
            gc = Nothing,
            fprefix = Nothing,
            prologue = Nothing,
            personality = Nothing,
            metadata = [],
            body = [MkBlock "entry" [
                -- Division by potentially zero value
                Operation (Assign (NamedRegister "div_result")) (BinaryOp SDiv (LInt 32) ( (LInt 100)) (LVar (Local (NamedRegister "input")))),
                -- Check for overflow (proper comparison)
                Operation (Assign (NamedRegister "overflow_check")) (icmp CNe (LInt 32) (LVar (Local (NamedRegister "input"))) ( (LInt 0))),
                Operation Discard (TerminatorOp (CondBr (LVar (Local (NamedRegister "overflow_check"))) ( (LPtr (Local (NamedRegister "error")))) ( (LPtr (Local (NamedRegister "normal"))))))
                ] (CondBr (LVar (Local (NamedRegister "overflow_check"))) ( (LPtr (Local (NamedRegister "error")))) ( (LPtr (Local (NamedRegister "normal"))))),
                    
                MkBlock "error" [
                    -- Unreachable after error
                    Operation Discard (TerminatorOp (Ret (LInt 32) ( (LInt (-1))))),
                    Operation Discard (TerminatorOp Unreachable)  -- This should never be reached
                ] (Ret (LInt 32) ( (LInt (-1)))),
                    
                MkBlock "normal" [
                    -- Phi node with single predecessor (edge case)
                    Operation (Assign (NamedRegister "phi_result")) (MiscOp (Phi (LInt 32) [
                        ( (LPtr (Local (NamedRegister "div_result"))),  (LPtr (Local (NamedRegister "entry"))))
                    ])),
                    Operation Discard (TerminatorOp (Ret (LInt 32) ( (LPtr (Local (NamedRegister "phi_result"))))))
                ] (Ret (LInt 32) ( (LPtr (Local (NamedRegister "phi_result")))))
            ],
            tags = []
        }
    ],
    tags = Nothing
}
{- 
-- Module testing memory management and allocation
moduleWithMemoryManagement : LModule
moduleWithMemoryManagement = MkLModule {
    dataLayout = Nothing,
    target = Nothing,
    text = [
        -- Declare malloc and free
        FunctionDecC $ MkFunctionDec {
            name = "malloc",
            symbolInfo = MkSymbolInfo (Just External) Nothing (Just Default) Nothing,
            callingConvention = Just C,
            returnAttrs = [NoAlias],
            returnType = LPtr,
            args = [MkFunctionArgSpec (LInt 64) [] Nothing],
            addressInfo = Nothing,
            alignment = Nothing,
            gc = Nothing,
            fprefix = Nothing,
            prologue = Nothing,
            tags = []
        },
        FunctionDecC $ MkFunctionDec {
            name = "free",
            symbolInfo = MkSymbolInfo (Just External) Nothing (Just Default) Nothing,
            callingConvention = Just C,
            returnAttrs = [],
            returnType = LVoid,
            args = [MkFunctionArgSpec LPtr [] Nothing],
            addressInfo = Nothing,
            alignment = Nothing,
            gc = Nothing,
            fprefix = Nothing,
            prologue = Nothing,
            tags = []
        },
        FunctionDefC $ MkFunctionDef {
            name = "memory_test",
            symbolInfo = MkSymbolInfo (Just External) Nothing (Just Default) Nothing,
            callingConvention = Just C,
            returnAttrs = [],
            returnType = LPtr,
            args = [MkFunctionArgSpec (LInt 32) [] (Just "size")],
            addressInfo = Nothing,
            addressSpace = Nothing,
            fnAttributes = [],
            section = Nothing,
            partition = Nothing,
            comdat = Nothing,
            alignment = Nothing,
            gc = Nothing,
            fprefix = Nothing,
            prologue = Nothing,
            personality = Nothing,
            metadata = [],
            body = [MkBlock "entry" [
                -- Convert size to 64-bit for malloc
                Operation (Assign (NamedRegister "size64")) (ConversionOp ZExt (MkWithType (LInt 32) (LVar (Local (NamedRegister "size")))) (LInt 64)),
                -- Allocate memory
                Operation (Assign (NamedRegister "ptr")) (MiscOp (FnCallOp (MkFnCall NoTail [] (Just C) [] Nothing 
                    (LFun LPtr [LInt 64]) 
                    ( (LPtr (Global "malloc"))) 
                    [MkWithType (LInt 64) (LVar (Local (NamedRegister "size64")))] []))),
                -- Check if allocation succeeded (proper pointer comparison)
                Operation (Assign (NamedRegister "is_null")) (icmp CEq LPtr (LVar (Local (NamedRegister "ptr"))) ( LNull)),
                Operation Discard (TerminatorOp (CondBr (LVar (Local (NamedRegister "is_null"))) ( (LPtr (Local (NamedRegister "alloc_failed")))) ( (LPtr (Local (NamedRegister "alloc_success"))))))
                ] (CondBr (LVar (Local (NamedRegister "is_null"))) ( (LPtr (Local (NamedRegister "alloc_failed")))) ( (LPtr (Local (NamedRegister "alloc_success")))))),
                
                MkBlock "alloc_failed" [
                    Operation Discard (TerminatorOp (Ret LPtr ( LNull)))
                ] (Ret LPtr ( LNull)),
                    
                MkBlock "alloc_success" [
                    -- Initialize allocated memory to zero
                    Operation Discard (MiscOp (FnCallOp (MkFnCall NoTail [] (Just C) [] Nothing 
                        (LFun LVoid [LPtr, LInt 8, LInt 64]) 
                        ( (LPtr (Global "llvm.memset.p0i8.i64"))) 
                        [
                            MkWithType LPtr (LVar (Local (NamedRegister "ptr"))),
                            MkWithType (LInt 8) ( (LInt 0)),
                            MkWithType (LInt 64) (LVar (Local (NamedRegister "size64")))
                        ] []))),
                Operation Discard (TerminatorOp (Ret LPtr (LVar (Local (NamedRegister "ptr")))))
            ] (Ret LPtr (LVar (Local (NamedRegister "ptr"))))],
            tags = []
        }
    ],
    tags = Nothing
}
-}
-- Module for testing intrinsic functions and LLVM builtins
export
moduleWithIntrinsics : LModule
moduleWithIntrinsics = MkLModule {
    dataLayout = Nothing,
    target = Nothing,
    text = [
        FunctionDefC $ MkFunctionDef {
            name = "main",
            symbolInfo = MkSymbolInfo (Just External) Nothing (Just Default) Nothing,
            callingConvention = Just C,
            returnAttrs = [],
            returnType = LFloating LDouble,
            args = [
                MkFunctionArgSpec (LFloating LDouble) [] (Just "x"),
                MkFunctionArgSpec (LFloating LDouble) [] (Just "y")
            ],
            addressInfo = Nothing,
            addressSpace = Nothing,
            fnAttributes = [],
            section = Nothing,
            partition = Nothing,
            comdat = Nothing,
            alignment = Nothing,
            gc = Nothing,
            fprefix = Nothing,
            prologue = Nothing,
            personality = Nothing,
            metadata = [],
            body = [MkBlock "entry" [
                    -- Call sqrt intrinsic
                    Operation (Assign (NamedRegister "sqrt_x")) (MiscOp (FnCallOp (MkFnCall NoTail [] (Just C) [] Nothing 
                        (LFun (LFloating LDouble) [LFloating LDouble]) 
                        ( (LPtr (Global "llvm.sqrt.f64"))) 
                        [MkWithType (LFloating LDouble) ( (LPtr (Local (NamedRegister "x"))))] []))),
                    -- Call sin intrinsic
                    Operation (Assign (NamedRegister "sin_y")) (MiscOp (FnCallOp (MkFnCall NoTail [] (Just C) [] Nothing 
                        (LFun (LFloating LDouble) [LFloating LDouble]) 
                        ( (LPtr (Global "llvm.sin.f64"))) 
                        [MkWithType (LFloating LDouble) ( (LPtr (Local (NamedRegister "y"))))] []))),
                    -- Call fma (fused multiply-add) intrinsic
                    Operation (Assign (NamedRegister "fma_result")) (MiscOp (FnCallOp (MkFnCall NoTail [] (Just C) [] Nothing 
                        (LFun (LFloating LDouble) [LFloating LDouble, LFloating LDouble, LFloating LDouble]) 
                        ( (LPtr (Global "llvm.fma.f64"))) 
                        [
                            MkWithType (LFloating LDouble) ( (LPtr (Local (NamedRegister "sqrt_x")))),
                            MkWithType (LFloating LDouble) ( (LPtr (Local (NamedRegister "sin_y")))),
                            MkWithType (LFloating LDouble) ( (LFloat "1.0"))
                        ] []))),
                    -- Check for NaN using intrinsic
                    Operation (Assign (NamedRegister "is_nan")) (MiscOp (FnCallOp (MkFnCall NoTail [] (Just C) [] Nothing 
                        (LFun (LInt 1) [LFloating LDouble]) 
                        ( (LPtr (Global "llvm.isnan.f64"))) 
                        [MkWithType (LFloating LDouble) ( (LPtr (Local (NamedRegister "fma_result"))))] []))),
                    -- Select result based on NaN check
                    Operation (Assign (NamedRegister "final_result")) (MiscOp (Select [] 
                        (MkWithType (LInt 1) ( (LPtr (Local (NamedRegister "is_nan")))))
                        (MkWithType (LFloating LDouble) ( (LFloat "0.0"))) 
                        (MkWithType (LFloating LDouble) ( (LPtr (Local (NamedRegister "fma_result"))))))),
                    Operation Discard (TerminatorOp (Ret (LFloating LDouble) ( (LPtr (Local (NamedRegister "final_result"))))))
                ] (Ret (LFloating LDouble) ( (LPtr (Local (NamedRegister "final_result")))))
            ],
            tags = []
        }
    ],
    tags = Nothing
}
export
-- Module for testing garbage collection and stack maps
moduleWithGC : LModule
moduleWithGC = MkLModule {
    dataLayout = Nothing,
    target = Nothing,
    text = [
        FunctionDefC $ MkFunctionDef {
            name = "gc_test",
            symbolInfo = MkSymbolInfo (Just External) Nothing (Just Default) Nothing,
            callingConvention = Just C,
            returnAttrs = [],
            returnType = LPtr,
            args = [MkFunctionArgSpec LPtr [] (Just "root")],
            addressInfo = Nothing,
            addressSpace = Nothing,
            fnAttributes = [],
            section = Nothing,
            partition = Nothing,
            comdat = Nothing,
            alignment = Nothing,
            gc = Just "shadow-stack",  -- Use shadow stack GC
            fprefix = Nothing,
            prologue = Nothing,
            personality = Nothing,
            metadata = [],
            body = [MkBlock "entry" [
                -- GC root declaration
                Operation Discard (MiscOp (FnCallOp (MkFnCall NoTail [] (Just C) [] Nothing 
                    (LFun LVoid [LPtr, LPtr]) 
                    ( (LPtr (Global "llvm.gcroot"))) 
                    [
                        MkWithType LPtr ( (LPtr (Local (NamedRegister "root")))),
                        MkWithType LPtr ( LNull)
                    ] []))),
                -- Potential GC safepoint
                Operation Discard (MiscOp (FnCallOp (MkFnCall NoTail [] (Just C) [] Nothing 
                    (LFun LVoid []) 
                    ( (LPtr (Global "llvm.experimental.gc.statepoint.p0f_isVoidf"))) 
                    [] []))),
                -- Read from GC pointer
                Operation (Assign (NamedRegister "value")) (MemoryOp (LoadRegular False LPtr
                    ( (LPtr (Local (NamedRegister "root"))))
                    Nothing False False False False Nothing Nothing Nothing False)),
                Operation Discard (TerminatorOp (Ret LPtr ( (LPtr (Local (NamedRegister "value"))))))
                ] 
                (Ret LPtr ( (LPtr (Local (NamedRegister "value")))))],
            tags = []
        }
    ],
    tags = Nothing
}
export
-- Module for testing constant expressions and global constructors
moduleWithConstants : LModule
moduleWithConstants = MkLModule {
    dataLayout = Nothing,
    target = Nothing,
    text = ([
        -- Global constructor
        GlobalDefC $ MkGVarDef {
            name = "llvm.global_ctors",
            symbolInfo = MkSymbolInfo (Just External) Nothing (Just Default) Nothing,
            threadLocality = Nothing,
            addressInfo = Nothing,
            addressSpace = Nothing,
            externallyInitialized = Nothing,
            isConst = False,
            gtpe = LArray 1 (LStruct [LInt 32, LPtr, LPtr]),
            initializer = Nothing,
            tags = []
        },
        -- Global destructor
        GlobalDefC $ MkGVarDef {
            name = "llvm.global_dtors",
            symbolInfo = MkSymbolInfo (Just External) Nothing (Just Default) Nothing,
            threadLocality = Nothing,
            addressInfo = Nothing,
            addressSpace = Nothing,
            externallyInitialized = Nothing,
            isConst = False,
            gtpe = LArray 1 (LStruct [LInt 32, LPtr, LPtr]),
            initializer = Nothing,
            tags = []
        },
        -- Complex constant expression
        GlobalDefC $ MkGVarDef {
            name = "complex_constant",
            symbolInfo = MkSymbolInfo (Just External) Nothing (Just Default) Nothing,
            threadLocality = Nothing,
            addressInfo = Nothing,
            addressSpace = Nothing,
            externallyInitialized = Nothing,
            isConst = True,
            gtpe = LPtr,
            initializer = Just LNull,
            tags = []
        },
        FunctionDefC $ MkFunctionDef {
            name = "constructor",
            symbolInfo = MkSymbolInfo (Just Internal) Nothing (Just Default) Nothing,
            callingConvention = Just C,
            returnAttrs = [],
            returnType = LVoid,
            args = [],
            addressInfo = Nothing,
            addressSpace = Nothing,
            fnAttributes = [],
            section = Nothing,
            partition = Nothing,
            comdat = Nothing,
            alignment = Nothing,
            gc = Nothing,
            fprefix = Nothing,
            prologue = Nothing,
            personality = Nothing,
            metadata = [],
            body = [MkBlock "entry" [
                -- Initialize global state
                Operation Discard (MemoryOp (StoreRegular False
                    (MkWithType (LInt 32) ( (LInt 42)))
                    ( (LPtr (Global "initialized_value")))
                    Nothing False False))
            ] RetVoid],
            tags = []
        },
        FunctionDefC $ MkFunctionDef {
            name = "destructor",
            symbolInfo = MkSymbolInfo (Just Internal) Nothing (Just Default) Nothing,
            callingConvention = Just C,
            returnAttrs = [],
            returnType = LVoid,
            args = [],
            addressInfo = Nothing,
            addressSpace = Nothing,
            fnAttributes = [],
            section = Nothing,
            partition = Nothing,
            comdat = Nothing,
            alignment = Nothing,
            gc = Nothing,
            fprefix = Nothing,
            prologue = Nothing,
            personality = Nothing,
            metadata = [],
            body = [MkBlock "entry" [
                -- Cleanup global state
                Operation Discard (MemoryOp (StoreRegular False
                    (MkWithType (LInt 32) ( (LInt 0)))
                    ( (LPtr (Global "initialized_value")))
                    Nothing False False)),
                Operation Discard (TerminatorOp RetVoid)
            ] RetVoid],
            tags = []
        },
        GlobalDefC $ MkGVarDef {
            name = "initialized_value",
            symbolInfo = MkSymbolInfo (Just External) Nothing (Just Default) Nothing,
            threadLocality = Nothing,
            addressInfo = Nothing,
            addressSpace = Nothing,
            externallyInitialized = Nothing,
            isConst = False,
            gtpe = LInt 32,
            initializer = Just (LInt 0),
            tags = []
        }
    ]),
    tags = Nothing
}

{- 
-- Module for testing comdats and linkage edge cases
moduleWithComdats : LModule
moduleWithComdats = MkLModule {
    dataLayout = Nothing,
    target = Nothing,
    text = [
        -- Global with weak linkage and comdat
        GlobalDefC $ MkGVarDef {
            name = "weak_global",
            symbolInfo = MkSymbolInfo (Just WeakODR) Nothing (Just Default) Nothing,
            threadLocality = Nothing,
            addressInfo = Nothing,
            addressSpace = Nothing,
            externallyInitialized = Nothing,
            isConst = False,
            gtpe = LInt 32,
            initializer = Just (LInt 100),
            tags = []
        },
        -- Function with linkonce linkage
        FunctionDefC $ MkFunctionDef {
            name = "linkonce_function",
            symbolInfo = MkSymbolInfo (Just LinkOnceODR) Nothing (Just Default) Nothing,
            callingConvention = Just C,
            returnAttrs = [],
            returnType = LInt 32,
            args = [],
            addressInfo = Nothing,
            addressSpace = Nothing,
            fnAttributes = [],
            section = Nothing,
            partition = Nothing,
            comdat = Just (Global "linkonce_function"),  -- Comdat group
            alignment = Nothing,
            gc = Nothing,
            fprefix = Nothing,
            prologue = Nothing,
            personality = Nothing,
            metadata = [],
            body = [MkBlock "entry" [
                Operation (Assign (NamedRegister "value")) (MemoryOp (LoadRegular False (LInt 32)
                    ( (LPtr (Global "weak_global")))
                    Nothing False False False False Nothing Nothing Nothing False))
            ] (Ret (LInt 32) (LVar (Local (NamedRegister "value"))))],
            tags = []
        },
        -- Available externally function (template instantiation)
        FunctionDefC $ MkFunctionDef {
            name = "template_instantiation",
            symbolInfo = MkSymbolInfo (Just Available) Nothing (Just Default) Nothing,
            callingConvention = Just C,
            returnAttrs = [],
            returnType = LVoid,
            args = [MkFunctionArgSpec (LInt 32) [] (Just "param")],
            addressInfo = Nothing,
            addressSpace = Nothing,
            fnAttributes = [],
            section = Nothing,
            partition = Nothing,
            comdat = Nothing,
            alignment = Nothing,
            gc = Nothing,
            fprefix = Nothing,
            prologue = Nothing,
            personality = Nothing,
            metadata = [],
            body = [MkBlock "entry" [
                -- Template-like operation
                Operation (Assign (NamedRegister "result")) (BinaryOp Mul (LInt 32) (LVar (Local (NamedRegister "param"))) (LVar (Local (NamedRegister "param")))),
                Operation Discard (MemoryOp (StoreRegular False
                    (MkWithType (LInt 32) (LVar (Local (NamedRegister "result"))))
                    ( (LPtr (Global "weak_global")))
                    Nothing False False))
            ] RetVoid),
            tags = []
        }
    ],
    tags = Nothing
}
-}
export
moduleWithComdats : LModule
moduleWithComdats = MkLModule {
    dataLayout = Nothing,
    target = Nothing,
    text = [],
    tags = Nothing
}
export
-- Module for stress testing with many functions and complex control flow
moduleStressTest : LModule
moduleStressTest = MkLModule {
    dataLayout = Nothing,
    target = Nothing,
    text = [
        -- Many small functions to test scale
        FunctionDefC $ MkFunctionDef {
            name = "stress_function_1",
            symbolInfo = MkSymbolInfo (Just External) Nothing (Just Default) Nothing,
            callingConvention = Just C,
            returnAttrs = [],
            returnType = LInt 32,
            args = [MkFunctionArgSpec (LInt 32) [] (Just "x")],
            addressInfo = Nothing,
            addressSpace = Nothing,
            fnAttributes = [],
            section = Nothing,
            partition = Nothing,
            comdat = Nothing,
            alignment = Nothing,
            gc = Nothing,
            fprefix = Nothing,
            prologue = Nothing,
            personality = Nothing,
            metadata = [],
            body = [MkBlock "entry" [
                Operation (Assign (NamedRegister "result")) (BinaryOp Add (LInt 32) (LVar (Local (NamedRegister "x"))) ( (LInt 1)))
            ] (Ret (LInt 32) (LVar (Local (NamedRegister "result"))))],
            tags = []
        },
        FunctionDefC $ MkFunctionDef {
            name = "stress_function_2",
            symbolInfo = MkSymbolInfo (Just External) Nothing (Just Default) Nothing,
            callingConvention = Just C,
            returnAttrs = [],
            returnType = LInt 32,
            args = [MkFunctionArgSpec (LInt 32) [] (Just "x")],
            addressInfo = Nothing,
            addressSpace = Nothing,
            fnAttributes = [],
            section = Nothing,
            partition = Nothing,
            comdat = Nothing,
            alignment = Nothing,
            gc = Nothing,
            fprefix = Nothing,
            prologue = Nothing,
            personality = Nothing,
            metadata = [],
            body = [MkBlock "entry" [
                Operation (Assign (NamedRegister "result")) (BinaryOp Mul (LInt 32) ( (LPtr (Local (NamedRegister "x")))) ( (LInt 2))),
                Operation Discard (TerminatorOp (Ret (LInt 32) ( (LPtr (Local (NamedRegister "result"))))))
            ] (Ret (LInt 32) ( (LPtr (Local (NamedRegister "result")))))],
            tags = []
        },
        -- Function with deeply nested control flow
        FunctionDefC $ MkFunctionDef {
            name = "deeply_nested",
            symbolInfo = MkSymbolInfo (Just External) Nothing (Just Default) Nothing,
            callingConvention = Just C,
            returnAttrs = [],
            returnType = LInt 32,
            args = [MkFunctionArgSpec (LInt 32) [] (Just "depth")],
            addressInfo = Nothing,
            addressSpace = Nothing,
            fnAttributes = [],
            section = Nothing,
            partition = Nothing,
            comdat = Nothing,
            alignment = Nothing,
            gc = Nothing,
            fprefix = Nothing,
            prologue = Nothing,
            personality = Nothing,
            metadata = [],
            body = [MkBlock "entry" [
                Operation (Assign (NamedRegister "cmp1")) (icmp CSGt (LInt 32) (LVar (Local (NamedRegister "depth"))) ( (LInt 0)))
            ] (CondBr (LVar (Local (NamedRegister "cmp1"))) (LVar (Local (NamedRegister "level1"))) (LVar (Local (NamedRegister "base")))),
                
            MkBlock "level1" [
                Operation (Assign (NamedRegister "cmp2")) (icmp CSGt (LInt 32) (LVar (Local (NamedRegister "depth"))) ( (LInt 1)))
            ] (CondBr (LVar (Local (NamedRegister "cmp2"))) (LVar (Local (NamedRegister "level2"))) (LVar (Local (NamedRegister "base")))),
                
            MkBlock "level2" [
                Operation (Assign (NamedRegister "cmp3")) (icmp CSGt (LInt 32) (LVar (Local (NamedRegister "depth"))) ( (LInt 2)))
            ] (CondBr (LVar (Local (NamedRegister "cmp3"))) (LVar (Local (NamedRegister "level3"))) (LVar (Local (NamedRegister "base")))),
                
            MkBlock "level3" [
                Operation (Assign (NamedRegister "sub")) (BinaryOp Sub (LInt 32) (LVar (Local (NamedRegister "depth"))) ( (LInt 1))),
                Operation (Assign (NamedRegister "recursive")) (MiscOp (FnCallOp (MkFnCall NoTail [] (Just C) [] Nothing 
                    (LFun (LInt 32) [LInt 32]) 
                    ( (LPtr (Global "deeply_nested"))) 
                    [MkWithType (LInt 32) (LVar (Local (NamedRegister "sub")))] [])))
            ] (JumpBr (LVar (Local (NamedRegister "base")))),
                
            MkBlock "base" [
                Operation (Assign (NamedRegister "phi")) (MiscOp (Phi (LInt 32) [
                    ( (LInt 0), LVar (Local (NamedRegister "entry"))),
                    ( (LInt 1), LVar (Local (NamedRegister "level1"))),
                    ( (LInt 2), LVar (Local (NamedRegister "level2"))),
                    (LVar (Local (NamedRegister "recursive")), LVar (Local (NamedRegister "level3")))
                ]))
            ] (Ret (LInt 32) (LVar (Local (NamedRegister "phi"))))],
            tags = []
        }
    ],
    tags = Nothing
}
----}

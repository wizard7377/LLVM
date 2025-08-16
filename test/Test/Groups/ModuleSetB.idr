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
                MkArgument LPtr [NoAlias] (Just "input"),   -- No alias
                MkArgument (LInt 32) [ZeroExt] (Just "size"),               -- Zero extend
                MkArgument LPtr [NoAlias] (Just "output")                -- No alias
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
            body = [MkBasicBlock "entry" [
                -- Memory copy with attributes
                MkLStatement Nothing (MiscOp (FnCallOp (MkFnCall NoTail [] (Just C) [] Nothing 
                    (LFun LVoid [LPtr, LPtr, LInt 32]) 
                    ( (LPtr (Global "llvm.memcpy.p0i8.p0i8.i32"))) 
                    [
                        MkWithType LPtr ( (LPtr (Local (id "output")))),
                        MkWithType LPtr ( (LPtr (Local (id "input")))),
                        MkWithType (LInt 32) ( (LPtr (Local (id "size"))))
                    ] []))),
                MkLStatement Nothing (TerminatorOp (Ret LPtr ( (LPtr (Local (id "output"))))))
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
            args = [MkArgument (LInt 32) [] (Just "input")],
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
            body = [MkBasicBlock "entry" [
                -- Division by potentially zero value
                MkLStatement (Assign (id "div_result")) (BinaryOp SDiv (LInt 32) ( (LInt 100)) (LVar (Local (id "input")))),
                -- Check for overflow (proper comparison)
                MkLStatement (Assign (id "overflow_check")) (icmp CNe (LInt 32) (LVar (Local (id "input"))) ( (LInt 0))),
                MkLStatement Nothing (TerminatorOp (CondBr (LVar (Local (id "overflow_check"))) ( (LPtr (Local (id "error")))) ( (LPtr (Local (id "normal"))))))
                ] (CondBr (LVar (Local (id "overflow_check"))) ( (LPtr (Local (id "error")))) ( (LPtr (Local (id "normal"))))),
                    
                MkBasicBlock "error" [
                    -- Unreachable after error
                    MkLStatement Nothing (TerminatorOp (Ret (LInt 32) ( (LInt (-1))))),
                    MkLStatement Nothing (TerminatorOp Unreachable)  -- This should never be reached
                ] (Ret (LInt 32) ( (LInt (-1)))),
                    
                MkBasicBlock "normal" [
                    -- Phi node with single predecessor (edge case)
                    MkLStatement (Assign (id "phi_result")) (MiscOp (Phi (LInt 32) [
                        ( (LPtr (Local (id "div_result"))),  (LPtr (Local (id "entry"))))
                    ])),
                    MkLStatement Nothing (TerminatorOp (Ret (LInt 32) ( (LPtr (Local (id "phi_result"))))))
                ] (Ret (LInt 32) ( (LPtr (Local (id "phi_result")))))
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
            args = [MkArgument (LInt 64) [] Nothing],
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
            args = [MkArgument LPtr [] Nothing],
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
            args = [MkArgument (LInt 32) [] (Just "size")],
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
            body = [MkBasicBlock "entry" [
                -- Convert size to 64-bit for malloc
                MkLStatement (Assign (id "size64")) (ConversionOp ZExt (MkWithType (LInt 32) (LVar (Local (id "size")))) (LInt 64)),
                -- Allocate memory
                MkLStatement (Assign (id "ptr")) (MiscOp (FnCallOp (MkFnCall NoTail [] (Just C) [] Nothing 
                    (LFun LPtr [LInt 64]) 
                    ( (LPtr (Global "malloc"))) 
                    [MkWithType (LInt 64) (LVar (Local (id "size64")))] []))),
                -- Check if allocation succeeded (proper pointer comparison)
                MkLStatement (Assign (id "is_null")) (icmp CEq LPtr (LVar (Local (id "ptr"))) ( LNull)),
                MkLStatement Nothing (TerminatorOp (CondBr (LVar (Local (id "is_null"))) ( (LPtr (Local (id "alloc_failed")))) ( (LPtr (Local (id "alloc_success"))))))
                ] (CondBr (LVar (Local (id "is_null"))) ( (LPtr (Local (id "alloc_failed")))) ( (LPtr (Local (id "alloc_success")))))),
                
                MkBasicBlock "alloc_failed" [
                    MkLStatement Nothing (TerminatorOp (Ret LPtr ( LNull)))
                ] (Ret LPtr ( LNull)),
                    
                MkBasicBlock "alloc_success" [
                    -- Initialize allocated memory to zero
                    MkLStatement Nothing (MiscOp (FnCallOp (MkFnCall NoTail [] (Just C) [] Nothing 
                        (LFun LVoid [LPtr, LInt 8, LInt 64]) 
                        ( (LPtr (Global "llvm.memset.p0i8.i64"))) 
                        [
                            MkWithType LPtr (LVar (Local (id "ptr"))),
                            MkWithType (LInt 8) ( (LInt 0)),
                            MkWithType (LInt 64) (LVar (Local (id "size64")))
                        ] []))),
                MkLStatement Nothing (TerminatorOp (Ret LPtr (LVar (Local (id "ptr")))))
            ] (Ret LPtr (LVar (Local (id "ptr"))))],
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
                MkArgument (LFloating LDouble) [] (Just "x"),
                MkArgument (LFloating LDouble) [] (Just "y")
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
            body = [MkBasicBlock "entry" [
                    -- Call sqrt intrinsic
                    MkLStatement (Assign (id "sqrt_x")) (MiscOp (FnCallOp (MkFnCall NoTail [] (Just C) [] Nothing 
                        (LFun (LFloating LDouble) [LFloating LDouble]) 
                        ( (LPtr (Global "llvm.sqrt.f64"))) 
                        [MkWithType (LFloating LDouble) ( (LPtr (Local (id "x"))))] []))),
                    -- Call sin intrinsic
                    MkLStatement (Assign (id "sin_y")) (MiscOp (FnCallOp (MkFnCall NoTail [] (Just C) [] Nothing 
                        (LFun (LFloating LDouble) [LFloating LDouble]) 
                        ( (LPtr (Global "llvm.sin.f64"))) 
                        [MkWithType (LFloating LDouble) ( (LPtr (Local (id "y"))))] []))),
                    -- Call fma (fused multiply-add) intrinsic
                    MkLStatement (Assign (id "fma_result")) (MiscOp (FnCallOp (MkFnCall NoTail [] (Just C) [] Nothing 
                        (LFun (LFloating LDouble) [LFloating LDouble, LFloating LDouble, LFloating LDouble]) 
                        ( (LPtr (Global "llvm.fma.f64"))) 
                        [
                            MkWithType (LFloating LDouble) ( (LPtr (Local (id "sqrt_x")))),
                            MkWithType (LFloating LDouble) ( (LPtr (Local (id "sin_y")))),
                            MkWithType (LFloating LDouble) ( (LFloat "1.0"))
                        ] []))),
                    -- Check for NaN using intrinsic
                    MkLStatement (Assign (id "is_nan")) (MiscOp (FnCallOp (MkFnCall NoTail [] (Just C) [] Nothing 
                        (LFun (LInt 1) [LFloating LDouble]) 
                        ( (LPtr (Global "llvm.isnan.f64"))) 
                        [MkWithType (LFloating LDouble) ( (LPtr (Local (id "fma_result"))))] []))),
                    -- Select result based on NaN check
                    MkLStatement (Assign (id "final_result")) (MiscOp (Select [] 
                        (MkWithType (LInt 1) ( (LPtr (Local (id "is_nan")))))
                        (MkWithType (LFloating LDouble) ( (LFloat "0.0"))) 
                        (MkWithType (LFloating LDouble) ( (LPtr (Local (id "fma_result"))))))),
                    MkLStatement Nothing (TerminatorOp (Ret (LFloating LDouble) ( (LPtr (Local (id "final_result"))))))
                ] (Ret (LFloating LDouble) ( (LPtr (Local (id "final_result")))))
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
            args = [MkArgument LPtr [] (Just "root")],
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
            body = [MkBasicBlock "entry" [
                -- GC root declaration
                MkLStatement Nothing (MiscOp (FnCallOp (MkFnCall NoTail [] (Just C) [] Nothing 
                    (LFun LVoid [LPtr, LPtr]) 
                    ( (LPtr (Global "llvm.gcroot"))) 
                    [
                        MkWithType LPtr ( (LPtr (Local (id "root")))),
                        MkWithType LPtr ( LNull)
                    ] []))),
                -- Potential GC safepoint
                MkLStatement Nothing (MiscOp (FnCallOp (MkFnCall NoTail [] (Just C) [] Nothing 
                    (LFun LVoid []) 
                    ( (LPtr (Global "llvm.experimental.gc.statepoint.p0f_isVoidf"))) 
                    [] []))),
                -- Read from GC pointer
                MkLStatement (Assign (id "value")) (MemoryOp (LoadRegular False LPtr
                    ( (LPtr (Local (id "root"))))
                    Nothing False False False False Nothing Nothing Nothing False)),
                MkLStatement Nothing (TerminatorOp (Ret LPtr ( (LPtr (Local (id "value"))))))
                ] 
                (Ret LPtr ( (LPtr (Local (id "value")))))],
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
            body = [MkBasicBlock "entry" [
                -- Initialize global state
                MkLStatement Nothing (MemoryOp (StoreRegular False
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
            body = [MkBasicBlock "entry" [
                -- Cleanup global state
                MkLStatement Nothing (MemoryOp (StoreRegular False
                    (MkWithType (LInt 32) ( (LInt 0)))
                    ( (LPtr (Global "initialized_value")))
                    Nothing False False)),
                MkLStatement Nothing (TerminatorOp RetVoid)
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
            body = [MkBasicBlock "entry" [
                MkLStatement (Assign (id "value")) (MemoryOp (LoadRegular False (LInt 32)
                    ( (LPtr (Global "weak_global")))
                    Nothing False False False False Nothing Nothing Nothing False))
            ] (Ret (LInt 32) (LVar (Local (id "value"))))],
            tags = []
        },
        -- Available externally function (template instantiation)
        FunctionDefC $ MkFunctionDef {
            name = "template_instantiation",
            symbolInfo = MkSymbolInfo (Just Available) Nothing (Just Default) Nothing,
            callingConvention = Just C,
            returnAttrs = [],
            returnType = LVoid,
            args = [MkArgument (LInt 32) [] (Just "param")],
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
            body = [MkBasicBlock "entry" [
                -- Template-like operation
                MkLStatement (Assign (id "result")) (BinaryOp Mul (LInt 32) (LVar (Local (id "param"))) (LVar (Local (id "param")))),
                MkLStatement Nothing (MemoryOp (StoreRegular False
                    (MkWithType (LInt 32) (LVar (Local (id "result"))))
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
            args = [MkArgument (LInt 32) [] (Just "x")],
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
            body = [MkBasicBlock "entry" [
                MkLStatement (Assign (id "result")) (BinaryOp Add (LInt 32) (LVar (Local (id "x"))) ( (LInt 1)))
            ] (Ret (LInt 32) (LVar (Local (id "result"))))],
            tags = []
        },
        FunctionDefC $ MkFunctionDef {
            name = "stress_function_2",
            symbolInfo = MkSymbolInfo (Just External) Nothing (Just Default) Nothing,
            callingConvention = Just C,
            returnAttrs = [],
            returnType = LInt 32,
            args = [MkArgument (LInt 32) [] (Just "x")],
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
            body = [MkBasicBlock "entry" [
                MkLStatement (Assign (id "result")) (BinaryOp Mul (LInt 32) ( (LPtr (Local (id "x")))) ( (LInt 2))),
                MkLStatement Nothing (TerminatorOp (Ret (LInt 32) ( (LPtr (Local (id "result"))))))
            ] (Ret (LInt 32) ( (LPtr (Local (id "result")))))],
            tags = []
        },
        -- Function with deeply nested control flow
        FunctionDefC $ MkFunctionDef {
            name = "deeply_nested",
            symbolInfo = MkSymbolInfo (Just External) Nothing (Just Default) Nothing,
            callingConvention = Just C,
            returnAttrs = [],
            returnType = LInt 32,
            args = [MkArgument (LInt 32) [] (Just "depth")],
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
            body = [MkBasicBlock "entry" [
                MkLStatement (Assign (id "cmp1")) (icmp CSGt (LInt 32) (LVar (Local (id "depth"))) ( (LInt 0)))
            ] (CondBr (LVar (Local (id "cmp1"))) (LVar (Local (id "level1"))) (LVar (Local (id "base")))),
                
            MkBasicBlock "level1" [
                MkLStatement (Assign (id "cmp2")) (icmp CSGt (LInt 32) (LVar (Local (id "depth"))) ( (LInt 1)))
            ] (CondBr (LVar (Local (id "cmp2"))) (LVar (Local (id "level2"))) (LVar (Local (id "base")))),
                
            MkBasicBlock "level2" [
                MkLStatement (Assign (id "cmp3")) (icmp CSGt (LInt 32) (LVar (Local (id "depth"))) ( (LInt 2)))
            ] (CondBr (LVar (Local (id "cmp3"))) (LVar (Local (id "level3"))) (LVar (Local (id "base")))),
                
            MkBasicBlock "level3" [
                MkLStatement (Assign (id "sub")) (BinaryOp Sub (LInt 32) (LVar (Local (id "depth"))) ( (LInt 1))),
                MkLStatement (Assign (id "recursive")) (MiscOp (FnCallOp (MkFnCall NoTail [] (Just C) [] Nothing 
                    (LFun (LInt 32) [LInt 32]) 
                    ( (LPtr (Global "deeply_nested"))) 
                    [MkWithType (LInt 32) (LVar (Local (id "sub")))] [])))
            ] (JumpBr (LVar (Local (id "base")))),
                
            MkBasicBlock "base" [
                MkLStatement (Assign (id "phi")) (MiscOp (Phi (LInt 32) [
                    ( (LInt 0), LVar (Local (id "entry"))),
                    ( (LInt 1), LVar (Local (id "level1"))),
                    ( (LInt 2), LVar (Local (id "level2"))),
                    (LVar (Local (id "recursive")), LVar (Local (id "level3")))
                ]))
            ] (Ret (LInt 32) (LVar (Local (id "phi"))))],
            tags = []
        }
    ],
    tags = Nothing
}
----}

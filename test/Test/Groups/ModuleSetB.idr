module Test.Groups.ModuleSetB

import Data.LLVM
import Data.LLVM.IR
import Data.LLVM.Write.Assembly
import Data.LLVM.Class
import Data.LLVM.IR.Builders.Math
import Test.Helper
import Data.LLVM.Write.Foreign


%hide Data.LLVM.IR.Builders.Core.emptyModule

export
moduleWithPhiAndSelect : LModule
moduleWithPhiAndSelect = MkLModule {
    dataLayout = Nothing,
    target = Nothing,
    text = [
        FunctionDefC $ MkFunctionDef {
            name = "phi_select_test",
            symbolInfo = MkSymbolInfo (Just External) Nothing (Just Default) Nothing,
            callingConvention = Just C,
            returnAttrs = [],
            returnType = LInt 32,
            args = [MkArgument (LInt 32) [] (Just "cond"), MkArgument (LInt 32) [] (Just "x"), MkArgument (LInt 32) [] (Just "y")],
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
            body = [
                MkBasicBlock "entry" [
                    "cond_bool" $<- (icmp CNe (LInt 32) (?^ "cond") (LConstE (LInt 0)))
                ] (CondBr (?^ "cond_bool") (?^ "bb_true") (?^ "bb_false")),
                MkBasicBlock "bb_true" [
                    "val_true" $<- (Add (LInt 32) (?^ "x") (LConstE (LInt 1)))
                ] (JumpBr (?^ "merge")),
                MkBasicBlock "bb_false" [
                    "val_false" $<- (Sub (LInt 32) (?^ "y") (LConstE (LInt 1)))
                ] (JumpBr (?^ "merge")),
                MkBasicBlock "merge" [
                    "phi_result" $<- (Phi (LInt 32) [((?^ "val_true"), (?^ "bb_true")), ((?^ "val_false"), (?^ "bb_false"))]),
                    "select_result" $<- (Select [] (MkWithType (LInt 32) (?^ "cond_bool")) (MkWithType (LInt 32) (?^ "val_true")) (MkWithType (LInt 32) (?^ "val_false")))
                ] (Ret (LInt 32) (?^ "select_result"))
            ],
            tags = []
        }
    ],
    tags = Nothing
}

export
moduleWithVectorAndAggregate : LModule
moduleWithVectorAndAggregate = MkLModule {
    dataLayout = Nothing,
    target = Nothing,
    text = [
        FunctionDefC $ MkFunctionDef {
            name = "vector_aggregate_test",
            symbolInfo = MkSymbolInfo (Just External) Nothing (Just Default) Nothing,
            callingConvention = Just C,
            returnAttrs = [],
            returnType = LInt 32,
            args = [MkArgument (LVector 4 (LInt 32)) [] (Just "vec"), MkArgument (LStruct [LInt 32, LInt 32]) [] (Just "s")],
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
            body = [
                MkBasicBlock "entry" [
                    -- Extract element 2 from vector
                    "elem2" $<- (ExtractElement ((4 :<> (:# 32)) <:> (?^ "vec")) ((:# 32) <:> (LConstE (LInt 2)))),
                    -- Insert value into vector at index 1
                    "vec2" $<- (InsertElement ((4 :<> (:# 32)) <:> (?^ "vec")) ((:# 32) <:> (?^ "elem2")) ((:# 32) <:> (LConstE (LInt 1)))),
                    -- Shuffle vector with itself
                    "shuffled" $<- (ShuffleVector ((4 :<> (:# 32)) <:> (?^ "vec2")) ((4 :<> (:# 32)) <:> (?^ "vec2")) ((4 :<> (:# 32)) <:> (LConstE (LVector [((:# 32) <:> (LInt 0)), ((:# 32) <:> (LInt 1)), ((:# 32) <:> (LInt 2)), ((:# 32) <:> (LInt 3))])))),
                    -- Extract value from struct
                    "x_val" $<- (ExtractValue (MkWithType (LStruct [LInt 32, LInt 32]) (?^ "s")) 0),
                    -- Insert value into struct
                    "s2" $<- (InsertValue (MkWithType (LStruct [LInt 32, LInt 32]) (?^ "s")) (MkWithType (LInt 32) (?^ "x_val")) 1)
                ] (Ret (LInt 32) (?^ "x_val"))
            ],
            tags = []
        }
    ],
    tags = Nothing
}

export
moduleWithCastsAndComparisons : LModule
moduleWithCastsAndComparisons = MkLModule {
    dataLayout = Nothing,
    target = Nothing,
    text = [
        FunctionDefC $ MkFunctionDef {
            name = "casts_comparisons_test",
            symbolInfo = MkSymbolInfo (Just External) Nothing (Just Default) Nothing,
            callingConvention = Just C,
            returnAttrs = [],
            returnType = LInt 1,
            args = [MkArgument (LInt 32) [] (Just "a"), MkArgument (LInt 32) [] (Just "b")],
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
            body = [
                MkBasicBlock "entry" [
                    -- Truncate a to i1
                    "a_trunc" $<- (Trunc NoSigned (MkWithType (LInt 32) (?^ "a")) (LInt 1)),
                    -- Zero extend b to i64
                    "b_zext" $<- (ZExt (MkWithType (LInt 32) (?^ "b")) (LInt 64)),
                    -- Compare a and b
                    "cmp" $<- (ICmp CEq (LInt 32) (?^ "a") (?^ "b")),
                    -- Bitcast a to i32 (no-op)
                    "a_bitcast" $<- (BitCast (MkWithType (LInt 32) (?^ "a")) (LInt 32))
                ] (Ret (LInt 1) (?^ "a_trunc"))
            ],
            tags = []
        }
    ],
    tags = Nothing
}

export
moduleWithConversionsAndMemory : LModule
moduleWithConversionsAndMemory = MkLModule {
    dataLayout = Nothing,
    target = Nothing,
    text = [
        FunctionDefC $ MkFunctionDef {
            name = "conversions_memory_test",
            symbolInfo = MkSymbolInfo (Just External) Nothing (Just Default) Nothing,
            callingConvention = Just C,
            returnAttrs = [],
            returnType = LInt 64,
            args = [MkArgument (LInt 32) [] (Just "a"), MkArgument (LInt 64) [] (Just "b")],
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
            body = [
                MkBasicBlock "entry" [
                    -- Sign extend a to i64
                    "a_sext" $<- (SExt (MkWithType (LInt 32) (?^ "a")) (LInt 64)),
                    -- Truncate b to i32
                    "b_trunc" $<- (Trunc NoSigned (MkWithType (LInt 64) (?^ "b")) (LInt 32)),
                    -- Compare a and b (signed less than)
                    "cmp" $<- (ICmp CSLt (LInt 32) (?^ "a") (?^ "b_trunc")),
                    -- Allocate i64 local
                    "local_ptr" $<- (Alloc (LInt 64) Nothing (Just 8) Nothing),
                    -- Store a_sext to local_ptr
                    $<< (StoreRegular False (MkWithType (LInt 64) (?^ "a_sext")) (?^ "local_ptr") (Just 8) False False),
                    -- Load from local_ptr
                    "loaded" $<- (LoadRegular False (LInt 64) (?^ "local_ptr") (Just 8) False False False False Nothing Nothing Nothing False)
                ] (Ret (LInt 64) (?^ "loaded"))
            ],
            tags = []
        }
    ],
    tags = Nothing
}

export
moduleWithAllComparisons : LModule
moduleWithAllComparisons = MkLModule {
    dataLayout = Nothing,
    target = Nothing,
    text = [
        FunctionDefC $ MkFunctionDef {
            name = "all_comparisons_test",
            symbolInfo = MkSymbolInfo (Just External) Nothing (Just Default) Nothing,
            callingConvention = Just C,
            returnAttrs = [],
            returnType = LInt 32,
            args = [MkArgument (LInt 32) [] (Just "x"), MkArgument (LInt 32) [] (Just "y")],
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
            body = [
                MkBasicBlock "entry" [
                    "eq" $<- (ICmp CEq (LInt 32) (?^ "x") (?^ "y")),
                    "ne" $<- (ICmp CNe (LInt 32) (?^ "x") (?^ "y")),
                    "ugt" $<- (ICmp CUGt (LInt 32) (?^ "x") (?^ "y")),
                    "uge" $<- (ICmp CUGe (LInt 32) (?^ "x") (?^ "y")),
                    "ult" $<- (ICmp CULt (LInt 32) (?^ "x") (?^ "y")),
                    "ule" $<- (ICmp CULe (LInt 32) (?^ "x") (?^ "y")),
                    "sgt" $<- (ICmp CSGt (LInt 32) (?^ "x") (?^ "y")),
                    "sge" $<- (ICmp CSGe (LInt 32) (?^ "x") (?^ "y")),
                    "slt" $<- (ICmp CSLt (LInt 32) (?^ "x") (?^ "y")),
                    "sle" $<- (ICmp CSLe (LInt 32) (?^ "x") (?^ "y"))
                ] (Ret (LInt 32) (LConstE (LInt 0)))
            ],
            tags = []
        }
    ],
    tags = Nothing
}

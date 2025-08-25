module Test.Groups.ModuleSetB

import Data.LLVM
import Data.LLVM.IR
import Data.LLVM.Write.Text.Encode
import Data.LLVM.Class
import Data.LLVM.Builders.Math
import Test.Helper
import Data.LLVM.Write.Foreign
import Data.Table

%hide Data.LLVM.Builders.Core.emptyModule
export
moduleWithPhiAndSelect : LModule
moduleWithPhiAndSelect = tempModule
export
moduleWithVectorAndAggregate : LModule
moduleWithVectorAndAggregate = tempModule
export
moduleWithCastsAndComparisons : LModule
moduleWithCastsAndComparisons = tempModule
export
moduleWithConversionsAndMemory : LModule
moduleWithConversionsAndMemory = tempModule
export
moduleWithAllComparisons : LModule
moduleWithAllComparisons = tempModule
{- 
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
            returnType = (:# 32),
            args = [MkArgument (:# 32) [] (Just "cond"), MkArgument (:# 32) [] (Just "x"), MkArgument (:# 32) [] (Just "y")],
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
                MkPair "entry" $ MkBasicBlock [
                    "cond_bool" $<- (icmp CNe (:# 32) (?^ "cond") ( (## 0)))
                ] (CondBr (?^ "cond_bool") (#^ "bb_true") (#^ "bb_false")),
                MkPair "bb_true" $ MkBasicBlock [
                    "val_true" $<- (Add (:# 32) (?^ "x") ( (## 1)))
                ] (JumpBr (#^ "merge")),
                MkPair "bb_false" $ MkBasicBlock [
                    "val_false" $<- (Sub (:# 32) (?^ "y") ( (## 1)))
                ] (JumpBr (#^ "merge")),
                MkPair "merge" $ MkBasicBlock [
                    "phi_result" $<- (Phi (:# 32) [((?^ "val_true"), (#^ "bb_true")), ((?^ "val_false"), (#^ "bb_false"))]),
                    "select_result" $<- (Select [] (MkWithType (:# 32) (?^ "cond_bool")) (MkWithType (:# 32) (?^ "val_true")) (MkWithType (:# 32) (?^ "val_false")))
                ] (Ret (:# 32) (?^ "select_result"))
            ],
            tags = neutral
        }
    ],
    tags = neutral
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
            returnType = (:# 32),
            args = [MkArgument (LVector 4 (:# 32)) [] (Just "vec"), MkArgument (LStruct [(:# 32), (:# 32)]) [] (Just "s")],
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
                MkPair "entry" $ MkBasicBlock [
                    -- Extract element 2 from vector
                    "elem2" $<- (ExtractElement ((4 :<> (:# 32)) <::> (?^ "vec")) ((:# 32) <::> ( (## 2)))),
                    -- Insert value into vector at index 1
                    "vec2" $<- (InsertElement ((4 :<> (:# 32)) <::> (?^ "vec")) ((:# 32) <::> (?^ "elem2")) ((:# 32) <::> ( (## 1)))),
                    -- Shuffle vector with itself
                    "shuffled" $<- (ShuffleVector ((4 :<> (:# 32)) <::> (?^ "vec2")) ((4 :<> (:# 32)) <::> (?^ "vec2")) ((4 :<> (:# 32)) <::> ( (.<> [((:# 32) <::> (## 0)), ((:# 32) <::> (## 1)), ((:# 32) <::> (## 2)), ((:# 32) <::> (## 3))])))),
                    -- Extract value from struct
                    "x_val" $<- (ExtractValue (MkWithType (LStruct [(:# 32), (:# 32)]) (?^ "s")) 0),
                    -- Insert value into struct
                    "s2" $<- (InsertValue (MkWithType (LStruct [(:# 32), (:# 32)]) (?^ "s")) (MkWithType (:# 32) (?^ "x_val")) 1)
                ] (Ret (:# 32) (?^ "x_val"))
            ],
            tags = neutral
        }
    ],
    tags = neutral
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
            returnType = (:# 1),
            args = [MkArgument (:# 32) [] (Just "a"), MkArgument (:# 32) [] (Just "b")],
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
                MkPair "entry" $ MkBasicBlock [
                    -- Truncate a to i1
                    "a_trunc" $<- (Trunc NoSigned (MkWithType (:# 32) (?^ "a")) (:# 1)),
                    -- Zero extend b to i64
                    "b_zext" $<- (ZExt (MkWithType (:# 32) (?^ "b")) (:# 64)),
                    -- Compare a and b
                    "cmp" $<- (ICmp CEq (:# 32) (?^ "a") (?^ "b")),
                    -- Bitcast a to i32 (no-op)
                    "a_bitcast" $<- (BitCast (MkWithType (:# 32) (?^ "a")) (:# 32))
                ] (Ret (:# 1) (?^ "a_trunc"))
            ],
            tags = neutral
        }
    ],
    tags = neutral
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
            returnType = :# 64,
            args = [MkArgument (:# 32) [] (Just "a"), MkArgument (:# 64) [] (Just "b")],
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
                MkPair "entry" $ MkBasicBlock [
                    -- Sign extend a to i64
                    "a_sext" $<- (SExt (MkWithType (:# 32) (?^ "a")) (## 64)),
                    -- Truncate b to i32
                    "b_trunc" $<- (Trunc NoSigned (MkWithType (:# 64) (?^ "b")) (:# 32)),
                    -- Compare a and b (signed less than)
                    "cmp" $<- (ICmp CSLt (:# 32) (?^ "a") (?^ "b_trunc")),
                    -- Allocate i64 local
                    "local_ptr" $<- (Alloc (:# 64) Nothing (Just 8) Nothing),
                    -- Store a_sext to local_ptr
                    $<< (StoreRegular False (MkWithType (:# 64) (?^ "a_sext")) (?^ "local_ptr") (Just 8) False False),
                    -- Load from local_ptr
                    "loaded" $<- (LoadRegular False (:# 64) (?^ "local_ptr") (Just 8) False False False False Nothing Nothing Nothing False)
                ] (Ret (:# 64) (?^ "loaded"))
            ],
            tags = neutral
        }
    ],
    tags = neutral
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
            returnType = (:# 32),
            args = [MkArgument (:# 32) [] (Just "x"), MkArgument (:# 32) [] (Just "y")],
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
                MkPair "entry" $ MkBasicBlock [
                    "eq" $<- (ICmp CEq (:# 32) (?^ "x") (?^ "y")),
                    "ne" $<- (ICmp CNe (:# 32) (?^ "x") (?^ "y")),
                    "ugt" $<- (ICmp CUGt (:# 32) (?^ "x") (?^ "y")),
                    "uge" $<- (ICmp CUGe (:# 32) (?^ "x") (?^ "y")),
                    "ult" $<- (ICmp CULt (:# 32) (?^ "x") (?^ "y")),
                    "ule" $<- (ICmp CULe (:# 32) (?^ "x") (?^ "y")),
                    "sgt" $<- (ICmp CSGt (:# 32) (?^ "x") (?^ "y")),
                    "sge" $<- (ICmp CSGe (:# 32) (?^ "x") (?^ "y")),
                    "slt" $<- (ICmp CSLt (:# 32) (?^ "x") (?^ "y")),
                    "sle" $<- (ICmp CSLe (:# 32) (?^ "x") (?^ "y"))
                ] (Ret (:# 32) ( (## 0)))
            ],
            tags = neutral
        }
    ],
    tags = neutral
}
-}
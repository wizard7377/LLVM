module Data.LLVM.Write.Foreign.Step


import Data.LLVM.Class
import Data.LLVM.IR
import Data.LLVM.CC
import Data.LLVM.CC
import public Control.Monad.State
import public Control.Monad.Either 
import public Data.LLVM.Write.Types
import System
import Data.String
import Data.LLVM.Write.Foreign.Monad
import Data.LLVM.Write.Foreign.Values
--%default covering 

%default partial 

public export 
Encode FCM Label LLVMBlock where 
  encode (NamedLabel n) = getBlock n
public export
Encode FCM Terminator CPtr where
	encode op = step "terminator opcode: " $ do
    cf <- popFun 
    cb <- popBuilder 
    res <- case op of
      Ret ty val => do 
        vv <- encode (MkWithType ty val)
        r <- liftFCM $ LLVMBuildRet cb vv 
        pure r
      RetVoid => do 
        liftFCM $ LLVMBuildRetVoid cb
      CondBr cond ifTrue ifFalse => do 
        cond' <- encode (withType (LType.LInt 1) cond)
        ifTrue' <- (encode ifTrue)
        ifFalse' <- (encode ifFalse)
        liftFCM $ LLVMBuildCondBr cb cond' ifTrue' ifFalse'

        
    pushBuilder cb 
    pushFun cf
    pure res
      
public export 
Encode FCM LExpr CPtr where
  encode e = step "Encoding expression" $ do
    builder <- popBuilder
    res <- case e of
      Phi ty pairs => throwError (InternalError "Phi node encoding not implemented")
      Select fm c t f => throwError (InternalError "Select encoding not implemented")
      Freeze v => throwError (InternalError "Freeze encoding not implemented")
      FnCallOp fncall => encode fncall
      LandingPad ty clauses => throwError (InternalError "LandingPad encoding not implemented")
      LandingPadCleanup ty clauses => throwError (InternalError "LandingPadCleanup encoding not implemented")
      CatchPad name v => throwError (InternalError "CatchPad encoding not implemented")
      CleanupPad name v => throwError (InternalError "CleanupPad encoding not implemented")
      Alloc ty mcount malign maddrspace => throwError (InternalError "Alloc encoding not implemented")
      LoadRegular v tpe addr align nontemp invLoad invGroup nonNull deref derefOrNull aligned noUndef => throwError (InternalError "LoadRegular encoding not implemented")
      LoadAtomic v tpe addr scope ordering align nontemp invGroup => throwError (InternalError "LoadAtomic encoding not implemented")
      StoreRegular v tpe addr align nontemp invGroup => throwError (InternalError "StoreRegular encoding not implemented")
      StoreAtomic v tpe addr scope ordering align invGroup => throwError (InternalError "StoreAtomic encoding not implemented")
      Fence scope ordering => throwError (InternalError "Fence encoding not implemented")
      FNeg ty v => do
        v' <- encode' (MkWithType ty v)
        liftFCM $ LLVMBuildFNeg builder v' "fneg"
      Add ty a b => do
        a' <- encode' (MkWithType ty a)
        b' <- encode' (MkWithType ty b)
        liftFCM $ LLVMBuildAdd builder a' b' "add"
      AddWrap wrap ty a b => throwError (InternalError "AddWrap encoding not implemented")
      FAdd fm ty a b => throwError (InternalError "FAdd encoding not implemented")
      Sub ty a b => do
        a' <- encode' (MkWithType ty a)
        b' <- encode' (MkWithType ty b)
        liftFCM $ LLVMBuildSub builder a' b' "sub"
      SubWrap wrap ty a b => throwError (InternalError "SubWrap encoding not implemented")
      FSub fm ty a b => throwError (InternalError "FSub encoding not implemented")
      Mul ty a b => do
        a' <- encode' (MkWithType ty a)
        b' <- encode' (MkWithType ty b)
        liftFCM $ LLVMBuildMul builder a' b' "mul"
      MulWrap wrap ty a b => throwError (InternalError "MulWrap encoding not implemented")
      FMul fm ty a b => throwError (InternalError "FMul encoding not implemented")
      UDiv ty a b => do
        a' <- encode' (MkWithType ty a)
        b' <- encode' (MkWithType ty b)
        liftFCM $ LLVMBuildUDiv builder a' b' "udiv"
      UDivExact ty a b => throwError (InternalError "UDivExact encoding not implemented")
      SDiv ty a b => do
        a' <- encode' (MkWithType ty a)
        b' <- encode' (MkWithType ty b)
        liftFCM $ LLVMBuildSDiv builder a' b' "sdiv"
      SDivExact ty a b => throwError (InternalError "SDivExact encoding not implemented")
      FDiv fm ty a b => throwError (InternalError "FDiv encoding not implemented")
      URem ty a b => do
        a' <- encode' (MkWithType ty a)
        b' <- encode' (MkWithType ty b)
        liftFCM $ LLVMBuildURem builder a' b' "urem"
      SRem ty a b => do
        a' <- encode' (MkWithType ty a)
        b' <- encode' (MkWithType ty b)
        liftFCM $ LLVMBuildSRem builder a' b' "srem"
      FRem fm ty a b => throwError (InternalError "FRem encoding not implemented")
      Shl ty a b => do
        a' <- encode' (MkWithType ty a)
        b' <- encode' (MkWithType ty b)
        liftFCM $ LLVMBuildShl builder a' b' "shl"
      ShlWrap wrap ty a b => throwError (InternalError "ShlWrap encoding not implemented")
      LShr ty a b => do
        a' <- encode' (MkWithType ty a)
        b' <- encode' (MkWithType ty b)
        liftFCM $ LLVMBuildLShr builder a' b' "lshr"
      LShrExact ty a b => throwError (InternalError "LShrExact encoding not implemented")
      AShr ty a b => do
        a' <- encode' (MkWithType ty a)
        b' <- encode' (MkWithType ty b)
        liftFCM $ LLVMBuildAShr builder a' b' "ashr"
      AShrExact ty a b => throwError (InternalError "AShrExact encoding not implemented")
      And ty a b => do

        a' <- encode' (MkWithType ty a)
        b' <- encode' (MkWithType ty b)
        liftFCM $ LLVMBuildAnd builder a' b' "and"
      Or ty a b => do

        a' <- encode' (MkWithType ty a)
        b' <- encode' (MkWithType ty b)
        liftFCM $ LLVMBuildOr builder a' b' "or"
      DisjointOr ty a b => throwError (InternalError "DisjointOr encoding not implemented")
      Xor ty a b => do

        a' <- encode' (MkWithType ty a)
        b' <- encode' (MkWithType ty b)
        liftFCM $ LLVMBuildXor builder a' b' "xor"
      InsertElement v1 v2 v3 => throwError (InternalError "InsertElement encoding not implemented")
      ShuffleVector v1 v2 v3 => throwError (InternalError "ShuffleVector encoding not implemented")
      ExtractElement v1 v2 => throwError (InternalError "ExtractElement encoding not implemented")
      ExtractValue v n => throwError (InternalError "ExtractValue encoding not implemented")
      InsertValue v1 v2 n => throwError (InternalError "InsertValue encoding not implemented")
      Trunc wrap v tpe => throwError (InternalError "Trunc encoding not implemented")
      ZExt v tpe => throwError (InternalError "ZExt encoding not implemented")
      SExt v tpe => throwError (InternalError "SExt encoding not implemented")
      FPTrunc fm v tpe => throwError (InternalError "FPTrunc encoding not implemented")
      FPExt fm v tpe => throwError (InternalError "FPExt encoding not implemented")
      FPToUi v tpe => throwError (InternalError "FPToUi encoding not implemented")
      FPToSi v tpe => throwError (InternalError "FPToSi encoding not implemented")
      UiToFP v tpe => throwError (InternalError "UiToFP encoding not implemented")
      SiToFP v tpe => throwError (InternalError "SiToFP encoding not implemented")
      PtrToInt v tpe => throwError (InternalError "PtrToInt encoding not implemented")
      BitCast v tpe => throwError (InternalError "BitCast encoding not implemented")
      AddrSpaceCast addr v tpe => throwError (InternalError "AddrSpaceCast encoding not implemented")
      ICmp cmp ty a b => throwError (InternalError "ICmp encoding not implemented")
      ICmpSign cmp ty a b => throwError (InternalError "ICmpSign encoding not implemented")
      FCmpOrd fm cmp ty a b => throwError (InternalError "FCmpOrd encoding not implemented")
      FCmpUnOrd fm cmp ty a b => throwError (InternalError "FCmpUnOrd encoding not implemented")
      FCmpFalse ty a b => throwError (InternalError "FCmpFalse encoding not implemented")
      FCmpTrue ty a b => throwError (InternalError "FCmpTrue encoding not implemented")
    pushBuilder builder
    pure res


export
Encode FCM LStatement CPtr where
    -- TODO:
    encode (MkLStatement (Just name) op _) = step "statement" $ do 
            res <- encode' op
            pushScope name res
            --TODO
            pure res
            
    encode (MkLStatement Nothing op _) = step "statement" $ do
            res <- encode' op
            pure res

public export
Encode FCM BasicBlock CPtr where
    encode (MkBasicBlock name statements term) = step ("BasicBlock: " ++ name) $ do 
        
        putMsg 15 ("aapending block to function")
        block <- getBlock name 
        f <- popFun 
        builder <- fcm $ LLVMCreateBuilderInContext !inCon
        _ <- fcm $ LLVMPositionBuilderAtEnd builder block
        putMsg 15 ("encoding block statements")
        (r, builder2) <- usingBuilder builder (traverse encode' statements) 
        (r2, builder3) <- usingBuilder builder2 (encode' term)
        pushFun f
        pure block

public export
[ArgsOf] Encode FCM Argument LLVMValue where
	encode (MkArgument ty attr name) = step ("arg spec") $ do 
            ty' <- encode' ty
            -- TODO: attr' <- encode att
            fv <- popFun
            v <- case name of 
              Just n => do 
                let v' = fcm $ LLVMGetLastParam fv.val
                pushScope' (Parameter n) v' 
                v'
              Nothing => fcm $ LLVMGetLastParam fv.val
            pushFun fv
            pure v
            -- TODO
public export 
[ArgInType] Encode FCM Argument (Maybe String, LLVMType) where
  encode (MkArgument ty attr name) = step ("arg spec") $ do 
            ty' <- encode' ty 
            pure (name, ty')
{-

define [linkage] [PreemptionSpecifier] [visibility] [DLLStorageClass]
       [cconv] [ret attrs]
       <ResultType> @<FunctionName> ([argument list])
       [(unnamed_addr|local_unnamed_addr)] [AddrSpace] [fn Attrs]
       [section "name"] [partition "name"] [comdat [($name)]] [align N]
       [gc] [prefix Constant] [prologue Constant] [personality Constant]
       (!name !N)* { ... }

 -}

numberEach : {default 0 ind : Nat} -> List a -> List (Nat, a)
numberEach {ind} [] = []
numberEach {ind} (x :: xs) = (ind, x) :: numberEach {ind = ind + 1} xs
public export
Encode FCM FunctionDef LLVMValue where
    encode (MkFunctionDef name _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ [] _) = throwError (EmptyFunction name)
    encode (
      MkFunctionDef 
        name symbolInfo callingConvention returnAttrs 
        returnType args 
        addressInfo addressSpace 
        fnAttributes section partition comdat alignment 
        gc fprefix prologue 
        personality metadata body tags) = step ("Function def: " ++ name) $ do 
            returnType' : CPtr <- encode' returnType
            args' : List (Maybe String, CPtr) <- traverse (encode @{ArgInType}) args
            args'' <- liftFCM $ toCList $ snd <$> args' 
            let argNames = fst <$> args'
            let argCount = length args 
            
            fty <- liftFCM $ LLVMFunctionType returnType' (prim__castPtr args'') (cast argCount) 0
            fv' <- liftFCM $ LLVMAddFunction !inMod name fty
            let fv = MkWorkingFunction argNames empty fv'
            (_, fv1) <- usingFun fv $ for_ args (encode @{ArgsOf}) 
            -- START HERE
            -- pushFun (MkWithScope (argNames fv)
            (body, fv2) <- usingFun fv1 (traverse encode' body)
            -- TODO: Personality
            -- TODO: Rest
            pure fv'

{-
declare [linkage] [visibility] [DLLStorageClass]
        [cconv] [ret attrs]
        <ResultType> @<FunctionName> ([argument list])
        [(unnamed_addr|local_unnamed_addr)] [align N] [gc]
        [prefix Constant] [prologue Constant]
 -}
public export
Encode FCM FunctionDec LLVMValue where
    encode (MkFunctionDec name symbolInfo callingConvention returnAttrs returnType args addressInfo alignment gc prefixdata prolouge tags) = step ("Function decl " ++ name) $ do
      args' : List (Maybe String, LLVMType) <- traverse (encode @{ArgInType}) args
      let argTys : List CPtr = snd <$> args' 
      let argNames : List (Maybe String) = fst <$> args'
      returnType' <- encode' returnType
      argList <- liftIO $ toCList' argTys
      ty' <- liftFCM $ LLVMFunctionType returnType' (prim__castPtr argList) (cast $ length args) 0
      seed <- liftFCM $ LLVMAddFunction !inMod name ty'
      -- TODO:
      pure seed
        



{-
@<Name> = [Linkage] [PreemptionSpecifier] [Visibility] [DLLStorageClass] [ThreadLocal] [(unnamed_addr|local_unnamed_addr)] alias <AliaseeTy>, <AliaseeTy>* @<Aliasee>
          [, partition "name"]
           -}

public export
Encode FCM Alias CPtr where 
	encode = ?h44

public export 
Encode FCM IFunc CPtr where 
	encode (MkIFunc name symInfo threadLocality addressInfo fTpe rTpe res tags) = ?h45
public export 
Encode FCM AttributeGroupDef CPtr where 
	encode = ?h46
public export 
Encode FCM LClause CPtr where 
    encode (GlobalDefC gdef) = encode' gdef
    encode (FunctionDefC fdef) = do 
      v : LLVMValue <- (encode fdef)
      pure v
    encode (FunctionDecC dec) = do 
      dec' : LLVMValue <- encode dec
      pure $ dec'
    encode (AliasC al) = encode' al
    encode (IFuncC f) = encode' f
    encode (MetadataC n m) = ?encmd
    encode (AttributeGroupC a) = encode' a
    encode (OtherC o) = ?encmo --encode' o
public export
Encode FCM LModule CPtr where
  encode (MkLModule layout target text tags) = do
    mod <- liftFCM $ LLVMModuleCreateWithName "MAIN" 
    modify (the changeState ({ cMod := Just mod }))
    case layout of 
      Just layout' => (liftFCM $ LLVMSetDataLayout mod layout') $> ()
      Nothing => pure ()
    case target of
      Just target' => (liftFCM $ LLVMSetTarget mod target') $> ()
      Nothing => pure ()
    _ <- traverse encode' text
    pure mod

    
    

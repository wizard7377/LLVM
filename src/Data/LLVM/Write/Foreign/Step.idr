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
%default covering 
maybeListToMap : Ord a => {default 0 index : Int} -> List (Maybe a) -> SortedMap a Int
maybeListToMap {index} ((Just x) :: xs) = mergeWith (\x, y => y) (insert x index empty) (maybeListToMap {index = index + 1} xs)
maybeListToMap {index} (Nothing :: xs) = maybeListToMap {index = index + 1} xs
maybeListToMap [] = empty
woScope : Functor f => {b : Type} -> f (WithScope {ref = b} a) -> f a
woScope = map (WithScope.value)
  
export
Encode FCM LType LLVMType where
  encode LType.LPtr = do
    gbl <- liftFCM $ LLVMGetGlobalContext
    ty <- liftFCM $ LLVMPointerTypeInContext gbl
    pure ty
  encode (LPtrTo ty) = do 
    ty' <- encode' ty
    liftFCM $ LLVMPointerType ty' 0
  encode LVoid = liftFCM $ LLVMVoidType
  encode (LFun ret args) = do 
    args' <- traverse encode' args
    ret' <- encode' ret
    argList <- liftIO $ toCList' args'
    let argList' = prim__castPtr argList
    liftFCM $ LLVMFunctionType ret' argList' (cast $ length args) 0
  encode (LType.LInt n) = liftFCM $ LLVMIntType $ cast n
  encode (LType.LVector n ty) = do 
    ty' <- encode' ty
    liftFCM $ LLVMVectorType ty' (cast n)
  encode LOpaque = ?enlopaq
  encode LLabel = fcm $ LLVMLabelTypeInContext !inCon
  encode LType.LToken = fcm $ LLVMTokenTypeInContext !inCon
  encode (LType.LArray size e) = do 
    e' <- encode' e 
    fcm $ LLVMArrayType2 e' $ cast size
  encode (LFloating f) = encode f
  encode (LType.LStruct fields) = do 
    fields' <- traverse encode' fields
    structTy <- fcm $ LLVMStructTypeInContext !inCon !(liftIO $ prim__castPtr <$> toCList' fields') (cast $ length fields) 0
    pure structTy
  encode (LPackedStruct fields) = do 
    fields' <- traverse encode' fields
    structTy <- fcm $ LLVMStructTypeInContext !inCon !(liftIO $ prim__castPtr <$> toCList' fields') (cast $ length fields) 1
    pure structTy
  encode (LFunVarArg ret args rest) = do 
    ret' <- encode' ret
    args' <- traverse encode' args
    rest' <- encode' rest
    argList <- liftIO $ toCList' (args' ++ [rest'])
    liftFCM $ LLVMFunctionType ret' (castPtr argList) (cast $ length args + 1) 1
  encode _ = ?h10
public export
[ewt] {a : Type} -> Encode FCM a CPtr => Encode FCM (WithType a) CPtr where 
    encode = ?h11

public export
{a : Type} -> Encode FCM a CPtr => Encode FCM (WithType a) CPtr where 
    encode = ?h12

mutual 
    export
    Encode FCM Metadata CPtr where
        encode (MetadataTuple elems) = do
            elems' <- traverse encode' elems
            elemList <- liftIO $ toCList' elems'
            liftFCM $ LLVMMDNodeInContext !inCon (castPtr elemList) (cast $ length elems)
        encode (MetadataNamed name) = do
            liftFCM $ LLVMMDStringInContext !inCon name (cast $ length name)
        encode (MetadataString str) = do
            liftFCM $ LLVMMDStringInContext !inCon str (cast $ length str)
        encode (MetadataValue (MkWithType ty expr)) = do
            ty' <- encode' ty
            expr' <- encode' (MkWithType ty expr)
            liftFCM $ LLVMValueAsMetadata expr'
        encode (MetadataCustom custom) = do
            liftFCM $ LLVMMDStringInContext !inCon custom (cast $ length custom)
    
    export
    Encode FCM (WithType LExpr) CPtr where
      encode (MkWithType ty v) = step "Make with type" $ do 
        ty' <- encode' ty
        case v of 
          LVar name => do 
            case name of 
              Local (NamedRegister n) => ?ewte9
              Local (UnnamedRegister n) => ?ewte10
              Global n => liftFCM $ LLVMGetNamedGlobal !inCon n
              Special n => liftFCM $ LLVMGetNamedGlobal !inCon n
              MetadataN n => ?ewte4
              LabelN n => ?ewte6
              IntrinsicN n => ?ewte7
              CustomN n => ?ewte8
              AttributeN n => ?ewte5
          LBool b => do 
            if b then liftFCM $ LLVMConstInt ty' 1 0 else liftFCM $ LLVMConstInt ty' 0 0
          LInt i => liftFCM $ LLVMConstInt ty' (cast i) 0
          LNull => liftFCM $ LLVMConstNull ty'
          LTerm.LFloat f => liftFCM $ LLVMConstReal ty' ?ewte2
          
          LTerm.LString s => liftFCM $ LLVMConstString s (cast $ length s + 1) 0
          LTerm.LPoison => liftFCM $ LLVMGetPoison ty'
          LTerm.LZero => liftFCM $ LLVMConstNull ty'
          _ => ?ewte3



export 
Encode FCM GVarDef CPtr where
	encode (MkGVarDef name symbolInfo threadLocality addressInfo addressSpace externallyInitialized constant ty init tags) = step "Global var def" $ do 
    ty' <- encode' ty
    seed <- liftFCM $ LLVMAddGlobal !inMod ty' name
    pure seed --TODO:
export
Encode FCM Attribute CPtr where 
    encode = ?h18
export
Encode FCM CaseBranch CPtr where
	encode = ?h19

export
Encode FCM InvokeCall CPtr where
	encode = ?h20

export
Encode FCM Wrapping CEnum where
	encode NoSigned = fcmPure $ (the CEnum (cast 1))
	encode NoUnsigned = fcmPure $ (the CEnum (cast 2))
	encode NoSignedUnsigned = fcmPure $ (the CEnum (cast 3))

export
Encode FCM (UnaryOpcode, LType, LExpr) CPtr where
	encode = ?h22

export
Encode FCM (BinaryOpcode, LType, LExpr, LExpr) CPtr where
	encode (op, ty, e0, e1) = step ("Binary opcode") $ do 
    e0' <- encode' $ MkWithType ty e0
    e1' <- encode' $ MkWithType ty e1
    ty' <- encode' ty
    case op of 
      Add => do
        builder <- popBuilder
        res <- liftFCM $ LLVMBuildAdd builder.value e0' e1' "add"
        pushBuilder builder
        pure res
      _ => ?h23

    

public export
Encode FCM BrCall CPtr where 
	encode = ?h24
public export
Encode FCM CatchSwitch CPtr where
	encode = ?h25
export
Encode FCM FastMathFlag CEnum where 
	encode FFast = fcmPure $ (the CEnum (cast 1))
	encode NoNaNs = fcmPure $ (the CEnum (cast 2))
	encode NoInfs = fcmPure $ (the CEnum (cast 4))
	encode NoSignedZeros = fcmPure $ (the CEnum (cast 8))

public export
Encode FCM TailCall CEnum where 
	encode Tail = fcmPure $ (the CEnum (cast 1))
	encode MustTail = fcmPure $ (the CEnum (cast 2))
	encode NoTail = fcmPure $ (the CEnum (cast 0))
public export
Encode FCM FnCall CPtr where
	encode = ?h28

public export
Encode FCM Terminator CPtr where
	encode = ?h29
public export
Encode FCM (ConversionOpCode, WithType LExpr, LType) CPtr where
	encode = ?h30

public export
Encode FCM VectorOpcode CPtr where
	encode = ?h31

export
Encode FCM AggregateOpcode CPtr where
	encode = ?h32
export 
Encode FCM MiscOpcode CPtr where
	encode = ?h33

emptyNode : VString 
emptyNode = "!{}"
nonTempNode : VString
nonTempNode = "!{ i32 1 }"
export
Encode FCM AtomicOrder CEnum where
	encode Unordered = fcmPure $ (the CEnum (cast 1))
	encode Monotonic = fcmPure $ (the CEnum (cast 2))
	encode Acquire = fcmPure $ (the CEnum (cast 4))
	encode Release = fcmPure $ (the CEnum (cast 5))
	encode AcquireRelease = fcmPure $ (the CEnum (cast 6))
	encode SequentiallyConsistent = fcmPure $ (the CEnum (cast 7))

export 
Encode FCM MemoryOpcode CPtr where
	encode = ?h35

export 
Encode FCM CatchClause CPtr where
	encode = ?h36

export 
Encode FCM ExceptOpcode CPtr where
	encode = ?h37

export
Encode FCM LOperation CPtr where
	encode op = step "operation" $ case op of
    TerminatorOp t => encode' t
    UnaryOp u t e => encode' (u, t, e)
    BinaryOp b t e0 e1 => encode' (b, t, e0, e1)
    VectorOp v => encode' v
    AggregateOp a => encode' a
    ConversionOp c e t => encode' (c, e, t)
    MiscOp m => encode' m
    MemoryOp m => encode' m
    ExceptOp e => encode' e
    

export
Encode FCM LStatement CPtr where
    encode (Operation (Assign name) op) = step "statement" $ do 
            res <- encode' op
            cBuild <- popBuilder 
            let cBuild' = { scope $= (insert name res) } cBuild 
            pushBuilder cBuild'
            --TODO
            pure res
            
    encode (Operation Discard op) = step "statement" $ do
            res <- encode' op
            pure res


public export
Encode FCM Block CPtr where
    encode (MkBlock name statements term) = step ("Block: " ++ name) $ do 
        f <- popFun
        putMsg 15 ("aapending block to function")
        block <- fcm $ LLVMAppendBasicBlockInContext !inCon f.value name
        builder <- fcm $ LLVMCreateBuilderInContext !inCon
        let builder' : WithScope {name = Register} ? = noScope builder
        _ <- fcm $ LLVMPositionBuilderAtEnd builder block
        putMsg 15 ("encoding block statements")
        (r, builder2) <- usingBuilder builder' (traverse encode' statements) 
        (r2, builder3) <- usingBuilder builder2 (encode' term)
        pushFun f
        pure block

public export
Encode FCM FunctionArgSpec (Maybe String, LLVMType) where
	encode (MkFunctionArgSpec ty attr name) = step ("arg spec") $ do 
            ty' <- encode' ty
            pure (name, ty')
            -- TODO


{-

define [linkage] [PreemptionSpecifier] [visibility] [DLLStorageClass]
       [cconv] [ret attrs]
       <ResultType> @<FunctionName> ([argument list])
       [(unnamed_addr|local_unnamed_addr)] [AddrSpace] [fn Attrs]
       [section "name"] [partition "name"] [comdat [($name)]] [align N]
       [gc] [prefix Constant] [prologue Constant] [personality Constant]
       (!name !N)* { ... }

 -}

public export
Encode FCM FunctionDef (WithScope {ref = Int} LLVMValue) where
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
            args' : List (Maybe String, CPtr) <- traverse encode args
            args'' <- liftFCM $ toCList $ snd <$> args' 
            let argNames = fst <$> args'
            let argCount = length args 
            fty <- liftFCM $ LLVMFunctionType returnType' (prim__castPtr args'') (cast argCount) 0
            fv <- liftFCM $ LLVMAddFunction !inMod name fty
            -- START HERE
            -- pushFun (MkWithScope (argNames fv)
            (body', fv') <- usingFun (MkWithScope (maybeListToMap argNames) fv) $ traverse encode' body
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
Encode FCM FunctionDec (WithScope {ref = Int} LLVMValue) where
    encode (MkFunctionDec name symbolInfo callingConvention returnAttrs returnType args addressInfo alignment gc prefixdata prolouge tags) = step ("Function decl " ++ name) $ do
      args' : List (Maybe String, CPtr) <- traverse encode args
      let argTys : List CPtr = snd <$> args' 
      let argNames : List (Maybe String) = fst <$> args'
      returnType' <- encode' returnType
      argList <- liftIO $ toCList' argTys
      ty' <- liftFCM $ LLVMFunctionType returnType' (prim__castPtr argList) (cast $ length args) 0
      seed <- liftFCM $ LLVMAddFunction !inMod name ty'
      -- TODO:
      pure (MkWithScope (maybeListToMap argNames) seed)
        



{-
@<Name> = [Linkage] [PreemptionSpecifier] [Visibility] [DLLStorageClass] [ThreadLocal] [(unnamed_addr|local_unnamed_addr)] alias <AliaseeTy>, <AliaseeTy>* @<Aliasee>
          [, partition "name"]
           -}

public export
Encode FCM Alias CPtr where 
	encode = ?h44

public export 
Encode FCM IFunc CPtr where 
	encode = ?h45
public export 
Encode FCM AttributeGroupDef CPtr where 
	encode = ?h46
public export 
Encode FCM LClause CPtr where 
    encode (GlobalDefC gdef) = encode' gdef
    encode (FunctionDefC fdef) = do 
      v : WithScope {ref = Int} LLVMValue <- (encode fdef)
      pure v.value
    encode (FunctionDecC dec) = do 
      dec' : WithScope {ref = Int} LLVMValue <- encode dec
      pure $ dec'.value
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

    
    
  
	


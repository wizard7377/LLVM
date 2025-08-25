module Data.LLVM.Builders.Inst

import Data.LLVM.IR.Core
import Data.Table
import Data.LLVM.IR.Core
import Data.LLVM.IR.Core
import Data.LLVM.IR.Program
import Data.LLVM.IR.Util


-- Setters for SymbolInfo
public export 
implementation Setter SymbolInfo (Maybe Linkage) where 
  setting f = { linkage $= f }

public export 
implementation Setter SymbolInfo (Maybe Visibility) where 
  setting f = { visibility $= f }

public export 
implementation Setter SymbolInfo (Maybe Preemption) where 
  setting f = { preemption $= f }

public export 
implementation Setter SymbolInfo (Maybe DLLStorage) where 
  setting f = { dllStorage $= f }

-- Setters for CaseBranch
public export 
implementation Setter CaseBranch LType where 
  setting f = { tpe $= f }

public export 
implementation Setter CaseBranch (LValue True) where 
  setting f = { value $= f }

public export 
implementation Setter CaseBranch Label where 
  setting f = { label $= f }

-- Setters for InvokeCall
public export 
implementation Setter InvokeCall (Maybe CallingConvention) where 
  setting f = { cc $= f }

public export 
implementation Setter InvokeCall (List Attribute) where 
  setting f = { returnAttrs $= f }

public export 
implementation Setter InvokeCall (Maybe AddressSpace) where 
  setting f = { addressSpace $= f }

public export 
implementation Setter InvokeCall LType where 
  setting f = { tpe $= f }

public export 
implementation Setter InvokeCall (LValue False) where 
  setting f = { fnval $= f }

public export 
implementation Setter InvokeCall (List (LValue False)) where 
  setting f = { args $= f }

public export 
implementation Setter InvokeCall Label where 
  setting f = { normal $=  f }

public export 
[UnwindSetter] Setter InvokeCall Label where 
  setting f = { unwind $=  f }

-- Setters for BrCall
public export 
implementation Setter BrCall (Maybe CallingConvention) where 
  setting f = { cc $= f }

public export 
implementation Setter BrCall (List Attribute) where 
  setting f = { returnAttrs $= f }

public export 
implementation Setter BrCall (Maybe AddressSpace) where 
  setting f = { addressSpace $= f }

public export 
implementation Setter BrCall LType where 
  setting f = { tpe $= f }

public export 
implementation Setter BrCall (LValue False) where 
  setting f = { fnval $= f }

public export 
implementation Setter BrCall (List (LValue False)) where 
  setting f = { args $= f }

public export 
implementation Setter BrCall Label where 
  setting f = { fallthrough $= f }

public export 
Setter BrCall (List Label) where 
  setting f = { indirect $= f }

-- Setters for FnCall
public export 
implementation Setter FnCall TailCall where 
  setting f = { tail $= f }

public export 
implementation Setter FnCall FastMath where 
  setting f = { fastMath $= f }

public export 
implementation Setter FnCall (Maybe CallingConvention) where 
  setting f = { cc $= f }

public export 
implementation [FnCallReturnAttrs] Setter FnCall (List Attribute) where 
  setting f = { returnAttrs $= f }

public export 
implementation [FnCallAddressSpace] Setter FnCall (Maybe AddressSpace) where 
  setting f = { addressSpace $= f }

public export 
implementation [FnCallType] Setter FnCall LType where 
  setting f = { tpe $= f }

public export 
implementation [FnCallFnVal] Setter FnCall (LValue False) where 
  setting f = { fnval $= f }

public export 
implementation Setter FnCall (List (WithType (LValue False))) where 
  setting f = { args $= f }

public export 
implementation Setter FnCall (List Attribute) where 
  setting f = { fnAttrs $= f }

-- Setters for LStatement
public export 
implementation Setter LStatement (Maybe Name) where 
  setting f = { target $= f }

public export 
implementation Setter LStatement LExpr where 
  setting f = { instruction $= f }

public export 
implementation Setter LStatement Annotation where 
  setting f = { metadata $= f }

-- Setters for BasicBlock
public export 
implementation Setter BasicBlock (List LStatement) where 
  setting f = { statements $= f }

public export 
implementation Setter BasicBlock Terminator where 
  setting f = { terminator $= f }

-- Setters for GVarDef
public export 
implementation Setter GVarDef String where 
  setting f = { name $= f }

public export 
implementation Setter GVarDef SymbolInfo where 
  setting f = { symbolInfo $= f }

public export 
implementation Setter GVarDef (Maybe ThreadLocality) where 
  setting f = { threadLocality $= f }

public export 
implementation [GVarDefAddressInfo] Setter GVarDef (Maybe AddressInfo) where 
  setting f = { addressInfo $= f }

public export 
implementation [GVarDefAddressSpace] Setter GVarDef (Maybe AddressSpace) where 
  setting f = { addressSpace $= f }

public export 
implementation Setter GVarDef (Maybe Bool) where 
  setting f = { externallyInitialized $= f }

public export 
implementation Setter GVarDef Bool where 
  setting f = { isConst $= f }

public export 
implementation [GVarDefType] Setter GVarDef LType where 
  setting f = { gtpe $= f }

public export 
implementation Setter GVarDef (Maybe (LValue True)) where 
  setting f = { initializer $= f }

public export 
implementation [GVarDefAnnotation] Setter GVarDef Annotation where 
  setting f = { tags $= f }

-- Setters for FunctionDef
public export 
implementation [FunctionDefName] Setter FunctionDef String where 
  setting f = { name $= f }

public export 
implementation [FunctionDefSymbolInfo] Setter FunctionDef SymbolInfo where 
  setting f = { symbolInfo $= f }

public export 
implementation [FunctionDefCallingConvention] Setter FunctionDef (Maybe CallingConvention) where 
  setting f = { callingConvention $= f }

public export 
implementation [FunctionDefReturnAttrs] Setter FunctionDef (List Attribute) where 
  setting f = { returnAttrs $= f }

public export 
implementation [FunctionDefReturnType] Setter FunctionDef LType where 
  setting f = { returnType $= f }

public export 
implementation Setter FunctionDef (List Argument) where 
  setting f = { args $= f }

public export 
implementation [FunctionDefAddressInfo] Setter FunctionDef (Maybe AddressInfo) where 
  setting f = { addressInfo $= f }

public export 
implementation [FunctionDefAddressSpace] Setter FunctionDef (Maybe AddressSpace) where 
  setting f = { addressSpace $= f }

public export 
implementation [FunctionDefFnAttributes] Setter FunctionDef (List Attribute) where 
  setting f = { fnAttributes $= f }

public export 
implementation Setter FunctionDef (Maybe String) where 
  setting f = { section $=  f }

public export 
[PartitionSetter] Setter FunctionDef (Maybe String) where 
  setting f = { partition $=  f }

public export 
[GCSetter] Setter FunctionDef (Maybe String) where 
  setting f = { gc $=  f }

public export 
implementation Setter FunctionDef (Maybe Name) where 
  setting f = { comdat $= f }

public export 
implementation Setter FunctionDef (Maybe Int) where 
  setting f = { alignment $= f }

public export 
implementation [FunctionDefPrefix] Setter FunctionDef (Maybe (LValue True)) where 
  setting f = { fprefix $= f }

public export 
[PrologueSetter] Setter FunctionDef (Maybe (LValue True)) where 
  setting f = { prologue $= f }

public export 
[PersonalitySetter] Setter FunctionDef (Maybe (LValue True)) where 
  setting f = { personality $= f }

public export 
implementation Setter FunctionDef (List Metadata) where 
  setting f = { metadata $= f }

public export 
implementation Setter FunctionDef (Table BasicBlock) where 
  setting f = { body $= f }

public export 
implementation [FunctionDefAnnotation] Setter FunctionDef Annotation where 
  setting f = { tags $= f }

-- Setters for FunctionDec
public export 
implementation [FunctionDecName] Setter FunctionDec String where 
  setting f = { name $= f }

public export 
implementation [FunctionDecSymbolInfo] Setter FunctionDec SymbolInfo where 
  setting f = { symbolInfo $= f }

public export 
implementation [FunctionDecCallingConvention] Setter FunctionDec (Maybe CallingConvention) where 
  setting f = { callingConvention $= f }

public export 
implementation [FunctionDecReturnAttrs] Setter FunctionDec (List Attribute) where 
  setting f = { returnAttrs $= f }

public export 
implementation [FunctionDecReturnType] Setter FunctionDec LType where 
  setting f = { returnType $= f }

public export 
implementation [FunctionDecArgs] Setter FunctionDec (List Argument) where 
  setting f = { args $= f }

public export 
implementation [FunctionDecAddressInfo] Setter FunctionDec (Maybe AddressInfo) where 
  setting f = { addressInfo $= f }

public export 
implementation [FunctionDecAlignment] Setter FunctionDec (Maybe Int) where 
  setting f = { alignment $= f }

public export 
[FunctionDecGC] Setter FunctionDec (Maybe String) where 
  setting f = { gc $= f }

public export 
[FunctionDecPrefix] Setter FunctionDec (Maybe (LValue True)) where 
  setting f = { fprefix $= f }

public export 
[FunctionDecPrologue] Setter FunctionDec (Maybe (LValue True)) where 
  setting f = { prologue $= f }

public export 
implementation [FunctionDecAnnotation] Setter FunctionDec Annotation where 
  setting f = { tags $= f }

-- Setters for Alias
public export 
implementation [AliasName] Setter Alias String where 
  setting f = { name $= f }

public export 
implementation [AliasSymbolInfo] Setter Alias SymbolInfo where 
  setting f = { symbolInfo $= f }

public export 
implementation [AliasThreadLocality] Setter Alias (Maybe ThreadLocality) where 
  setting f = { threadLocality $= f }

public export 
implementation [AliasAddressInfo] Setter Alias (Maybe AddressInfo) where 
  setting f = { addressInfo $= f }

public export 
implementation [AliasType] Setter Alias LType where 
  setting f = { aliasTpe $= f }

public export 
implementation [AliasTarget] Setter Alias String where 
  setting f = { aliasee $= f }

public export 
implementation [AliasAnnotation] Setter Alias Annotation where 
  setting f = { tags $= f }

-- Setters for IFunc
public export 
implementation [IFuncName] Setter IFunc String where 
  setting f = { name $= f }

public export 
implementation [IFuncSymbolInfo] Setter IFunc SymbolInfo where 
  setting f = { symbolInfo $= f }

public export 
implementation [IFuncThreadLocality] Setter IFunc (Maybe ThreadLocality) where 
  setting f = { threadLocality $= f }

public export 
implementation [IFuncAddressInfo] Setter IFunc (Maybe AddressInfo) where 
  setting f = { addressInfo $= f }

public export 
implementation [IFuncFunType] Setter IFunc LType where 
  setting f = { funTpe $= f }

public export 
[IFuncResType] Setter IFunc LType where 
  setting f = { resTpe $= f }

public export 
implementation [IFuncResolver] Setter IFunc String where 
  setting f = { resolver $= f }

public export 
implementation [IFuncAnnotation] Setter IFunc Annotation where 
  setting f = { tags $= f }

-- Setters for AttributeGroupDef
public export 
implementation Setter AttributeGroupDef Nat where 
  setting f = { name $= f }

public export 
implementation [AttributeGroupDefAttrs] Setter AttributeGroupDef (List Attribute) where 
  setting f = { attrs $= f }

-- Setters for LModule
public export 
implementation Setter LModule (Maybe String) where 
  setting f = { dataLayout $=  f }

public export 
[TargetSetter] Setter LModule (Maybe String) where 
  setting f = { target $=  f }

public export 
implementation Setter LModule (List LClause) where 
  setting f = { text $= f }

public export 
implementation [LModuleAnnotation] Setter LModule Annotation where 
  setting f = { tags $= f }



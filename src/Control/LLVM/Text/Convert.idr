module Control.LLVM.Text.Convert 

import Control.LLVM.Stage
import Control.LLVM.Code
  
export
||| Go from an LModule to LLVM assembly in a Code object.
convert : LModule -> Stage Code
convert m = do 
    showMsg "Converting to LLVM assembly"
    b' : VString <- the (Stage ?) $ pure $ runATM $ encode m
    let c = stringToCode $ show b'
    pure c

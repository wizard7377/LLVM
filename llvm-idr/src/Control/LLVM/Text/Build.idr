module Control.LLVM.Text.Build

import Control.LLVM.Stage
import Control.LLVM.Code
import Control.LLVM.Text.Convert
import Control.LLVM.Text.Assemble
import Control.LLVM.Text.Optimize
import Control.LLVM.Text.Link
import Control.LLVM.Text.Combine
import Control.LLVM.Text.Compile

export
compile : Bytecode -> Stage Code 
compile bc = do 
    context <- ask
    let mods = bc.modules
    mods' <- traverseStage (\(_, x) => convert x) mods
    modsBc <- traverseStage assemble mods'
    let extraBc = snd <$> bc.extrabitCode
    let extraBc' = bufferToCode <$> extraBc
    bcAll <- combine (extraBc' <+> modsBc)
    bcOpt <- optimize bcAll
    bcObj <- 
        if context.skipNative then 
            compileObj bcOpt
        else 
            ?noskip
    linked <- link bcObj
    let mainFile = context.output
    res <- liftIO $ codeToFile linked mainFile
    pure linked
    
    

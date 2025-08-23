module System.LLVM.Link
import System.LLVM.Common
import Data.LLVM.IR.Program
import System
import System.LLVM.Stage
intercalate : String -> List String -> String
intercalate sep [] = ""
intercalate sep [x] = x
intercalate sep (x :: xs) = x ++ foldl (\acc => \y => acc ++ sep ++ y) "" xs
export
linkLLVM : {auto context : Context} -> List String -> Compile String
linkLLVM {context} inputs = do
    showMsg $ "Linking modules: " ++ (intercalate ", " inputs)
    let inputs' = (map (\i => context.tempDir <+> i <+> ".bc") inputs) ++ context.extraBc
    let output' = context.tempDir <+> context.output <+> ".bc"
    let cmd : String = 
        case context.mainModule of
            Just mainMod' => "llvm-link -o " <+> (context.buildDir <+> context.output <+> ".bc") <+> " --override " <+> mainMod' <+> " " <+> (intercalate " " inputs')
            Nothing => "llvm-link -o " <+>  (context.buildDir <+> context.output <+> ".bc") <+> " " <+> (intercalate " " inputs')
    (out, r) <- runCmd cmd
    if r == 0 
        then pure context.output 
        else throwError $ LinkError out



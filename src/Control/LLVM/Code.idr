module Control.LLVM.Code

import System
import System.File
import Data.Buffer
import Control.Monad.Error.Interface

public export 
data Code : Type where
    CodeFile : String -> Code
    CodeBuffer : Buffer -> Code 
    CodeString : String -> Code

export 
codeToBuffer : Code -> IO (Either FileError Buffer)
codeToBuffer (CodeBuffer b) = pure $ Right b
codeToBuffer (CodeFile f) = createBufferFromFile f
codeToBuffer (CodeString s) = do 
    let sl : Int = cast $ length s 
    Just b <- newBuffer sl | _ => ?todo0 
    setString b sl s 
    pure $ Right b

export 
codeToFile : Code -> String -> IO (Either FileError ())
codeToFile (CodeBuffer b) f = do 
    s <- rawSize b 
    r <- writeBufferToFile f b s 
    case r of 
      Left (e, _) => pure $ Left e
      Right v => pure $ Right v
codeToFile (CodeFile f) out = do 
  r <- copyFile f out
  case r of 
    Left (e, _) => pure $ Left e
    Right v => pure $ Right v
codeToFile (CodeString s) f = liftIO $ writeFile f s
export 
bufferToCode : Buffer -> Code
bufferToCode = CodeBuffer
export
fileToCode : String -> Code
fileToCode = CodeFile
export 
stringToCode : String -> Code
stringToCode = CodeString

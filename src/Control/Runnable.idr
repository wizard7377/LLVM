module Control.Runnable

public export 
interface Monoid log => Runnable prg res err env st log where
    run : prg -> env -> st -> IO (Either err res, log, st)

export 
implementation Monoid l => Runnable (IO a) a err env st l where 
    run prg _ st = do
        res : a <- prg
        pure (Right res, neutral, st)


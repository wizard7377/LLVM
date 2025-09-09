module Data.Util 

public export
castTo : (0 r : Type) -> {a : Type} -> Cast a r => (x : a) -> r
castTo r x = the r $ cast $ the a x 

module Data.Table

public export %inline %tcinline
Table : Type -> Type 
Table a = List (String, a)

public export %inline %tcinline
Entry : Type -> Type 
Entry a = (String, a)
tableLookup : String -> Table a -> Maybe a
tableLookup key [] = Nothing
tableLookup key ((k, v) :: xs) = if k == key then Just v else tableLookup key xs

tableInsert : String -> a -> Table a -> Table a 
tableInsert k v t = (k, v) :: t

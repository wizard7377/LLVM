# Some useful opereators / classes

`walk`: more extensive `cast`
`$$` : `$` + `go`
`\@\@` : func app + go

## In `../src/Data/LLVM/IR/Builders/Ops.idr`

`globalDef`
`alias`
`mkModule`

## `src/Data/LLVM/IR/Builders/Helper.idr`

`switch`

## `Core`

`withType`
`local : String -> Name`
`$@ : String -> LExpr` (local name)
`global`
`$* : Name -> LExpr` 
`$<- : Name -> LOperation -> LStatement`
`discard` discard value
`symbolInfo`
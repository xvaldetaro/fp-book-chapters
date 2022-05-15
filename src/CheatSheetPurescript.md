### Records and Row Types
Check out fp-book page 1288 for a good example.
Check out `client` repo CreateUserRequest.createUser for more advanced usage
```purescript
type MyRow r = (rowField1 :: String | r)
type MyRow2 r = (row2Field1 :: Number | r)
-- the () means we pass an empty row to its `r` type
newtype RecordWithDefinedRow = RecordWithDefinedRow { | MyRow (MyRow2 ()) }

type MyExtensibleRecord = { field1 :: String, field2 :: Number | r }
-- instantiate like a normal record "
recordInstance = { field1: "asdf", field2: 12.0}
-- update the record with:
updatedRecordNewInstance = recordInstance { field1 = "xcv" }

-- deleting a field from Record (install purescript-record)
Record.delete (Proxy :: Proxy "field1") recordInstance
-- inserting a field in Record
Record.insert (Proxy ::  _ "field3") "asdf" recordInstance

```

### Pattern Matching
#### Getting subfield and whole variable
```purescript
foo :: Array Int -> Tuple (Array Int) Int
foo wholeArray@(x : xs) = Tuple wholeArray x

```
#### Case expressions
```purescript
case x of
  1 -> "is 1"
  2 -> "is 2"
  _ -> "something else
```

### Do notation
#### Multiline let block
The block needs 3x indentation:
```purescript
main = do
  x <- monadicComputation
  let
    y = do
      monadicComp2
      monadicComp3
  pure unit
```

### Type Holes, Typed Hole
In order to make things compile, use undefined. Need to install `undefined` with spago.
Hover mouse over error to see what the type is.

```purescript
main :: Effect Unit
main = do
  x <- myMonadicComputation :: ?x
  let y = nonMonadicFunction :: ?x
  undefined
```
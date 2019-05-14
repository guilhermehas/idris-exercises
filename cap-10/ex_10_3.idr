import Data.Vect

-- shape --

data Shape = Triangle Double Double
           | Rectangle Double Double
           | Circle Double

-- store --

infixr 5 .+.

data Schema = SString
            | SInt
            | SChar
            | (.+.) Schema Schema

SchemaType : Schema -> Type
SchemaType SString = String
SchemaType SInt = Int
SchemaType SChar = Char
SchemaType (sc1 .+. sc2) = (SchemaType sc1, SchemaType sc2)

record DataStore (schema : Schema) where
  constructor MkData
  size : Nat
  items : Vect size (SchemaType schema)

empty : DataStore schema
empty = MkData 0 []

addToStore : SchemaType schema -> DataStore schema -> DataStore schema
addToStore value (MkData _ items) = MkData _ (value :: items)

data StoreView : DataStore schema -> Type where
  SNil : StoreView empty
  SAdd : (rec : StoreView store) -> StoreView (addToStore value store)

storeViewHelp : (items : Vect size (SchemaType schema)) -> StoreView (MkData size items)
storeViewHelp [] = SNil
storeViewHelp (val :: xs) = SAdd (storeViewHelp xs)

storeView : (store : DataStore schema) -> StoreView store
storeView (MkData _ items) = storeViewHelp items

-- exercise 1 --

getValues : DataStore (SString .+. val_schema) -> List (SchemaType val_schema)
getValues stores with (storeView stores)
  getValues empty | SNil = []
  getValues (addToStore (key, value) stores) | (SAdd rec) = value :: getValues stores | rec

testStore : DataStore (SString .+. SInt)
testStore = addToStore ("First", 1) $
            addToStore ("Second", 2) $
            empty

area : Shape -> Double
area (Triangle x y) = x * y / 2
area (Rectangle x y) = x * y
area (Circle r) = 3.14 * r * r

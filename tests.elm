import Graphics.Element exposing ( show )

import Date
import Dict
import Maybe exposing (..)

type Key = StringKey String | IntKey Int
type Cell = StringCell String | FloatCell Float | IntCell Int | DateCell Date.Date | EmptyCell

type alias IntIndexedDict = Dict.Dict Int Element
type alias StringIndexedDict = Dict.Dict String Element
type Table = IntTable IntIndexedDict | StringTable StringIndexedDict | EmptyTable

type Element = ACell Cell | ATable Table

getElement : Table -> Key -> Maybe Element
getElement table key = case table of
  IntTable t -> case key of
    IntKey k -> Dict.get k t
    _ -> Nothing
  StringTable t -> case key of
    StringKey k -> Dict.get k t
    _ -> Nothing
  EmptyTable -> Nothing

getElementFromElement : Element -> Key -> Maybe Element
getElementFromElement el key = case el of
  ATable t -> getElement t key
  _ -> Nothing

getElementWithListOfKeys : Element -> List Key -> Maybe Element
getElementWithListOfKeys element listOfKeys = case element of
  ATable t -> case listOfKeys of
    key::rest -> (getElementFromElement element key) `andThen` (\el -> getElementWithListOfKeys el rest)
    [] -> Just element
  ACell c -> case listOfKeys of
    [] -> Just element
    _ -> Nothing

getElement2 : Table -> Key -> Key -> Maybe Element
getElement2 table key1 key2 =
  getElementWithListOfKeys (ATable table) [key1, key2]

getElementFromLookupResult : Table -> Maybe Element -> Maybe Element
getElementFromLookupResult table maybeEl = case maybeEl of
  Just anEl -> case anEl of
    ACell cell -> case cell of
      StringCell v -> getElement table (StringKey v)
      IntCell v -> getElement table (IntKey v)
      _ -> Nothing
    _ -> Nothing
  _ -> Nothing

insert : Key -> Element -> Table -> Table
insert key el table = case table of
  IntTable t -> case key of
    IntKey k -> IntTable (Dict.insert k el t)
    _ -> table
  StringTable t -> case key of
    StringKey k -> StringTable (Dict.insert k el t)
    _ -> table
  EmptyTable -> case key of
    IntKey k -> IntTable (Dict.singleton k el)
    StringKey k -> StringTable (Dict.singleton k el)

tableFromList : List (Key, Element) -> Table
tableFromList list =
  List.foldl (\(key, el) table -> insert key el table) EmptyTable list

elFromCellList : List (Key, Cell) -> Element
elFromCellList list =
  let elList = List.map (\(k, c) -> (k, ACell c)) list
  in
    ATable (tableFromList elList)

-- activities : List (Key, Cell)
activities =
  [ (IntKey 1,StringCell "Writing")
  , (IntKey 2,StringCell "Programming")
  , (IntKey 3,StringCell "Washing dishes")
  ]

-- hours : List (Key, Cell)
hours =
  [ (IntKey 1,FloatCell 1.5)
  , (IntKey 2,FloatCell 4)
  , (IntKey 3,FloatCell 0.5)
  ]

-- categories : List (Key, Cell)
categories =
  [ (StringKey "Programming",StringCell "Fun")
  , (StringKey "Writing",StringCell "Fun")
  , (StringKey "Washing dishes",StringCell "Not Fun")
  , (StringKey "Cooking",StringCell "Fun")
  ]

t1 = tableFromList [ (StringKey "Activities", elFromCellList activities), (StringKey "Hours", elFromCellList hours) ]

e = elFromCellList categories
t2 = case e of
  ATable t -> t
  _ -> EmptyTable


lookupResults = List.map (\i -> getElement2 t1 (StringKey "Activities") (IntKey i)) [1..5]
cats = List.map (\r -> getElementFromLookupResult t2 r) lookupResults

main =
  show cats

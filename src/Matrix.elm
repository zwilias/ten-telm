module Matrix
    exposing
        ( Matrix
        , create
        , empty
        , get
        , row
        , column
        , map
        , toList
        , indexedMap
        , flipHorizontal
        , flipVertical
        , Dimension
        , dimension
        , overlayMaybe
        , foldl
        , rotate
        , transpose
        , constant
        )

{-| Datastructure for managing rectangular data.


# Structures
The major structures that are used throughout the Matrix module.

As noted, Matrix currently is not an opaque type, but rather is exposing its
internal data structure as an `Array ( Array a )`. For now, this makes it
easier to do advanced operations on the contents, until a proper internal type
is foreseen.
@docs Matrix, Dimension

# Creation
@docs empty, create, constant

# Inspection
@docs get, row, column, dimension, toList

# Transformation
@docs map, indexedMap, overlayMaybe, foldl

# Manipulation
@docs rotate, transpose, flipHorizontal, flipVertical

-}

import Array exposing (Array)
import Maybe.Extra
import Array.Extra


{-| A Matrix is really just an array of arrays. Might change this into an
opaque type later on, since the internals is really none of your business.
-}
type alias Matrix a =
    Array (Array a)


{-| A Matrix has a dimension meaning it has a certain number of rows and
columns. These are expressed as an integer height and width.
-}
type alias Dimension =
    { width : Int, height : Int }


{-| Returns a Dimension for a Matrix.

```
Matrix.empty
    |> Matrix.dimension
    == Dimension { width = 0, height =  0}


aMatrix : Matrix Int
aMatrix =
    Matrix.constant 10 15 1


aMatrix
    |> Matrix.dimension
    == Dimension { width = 10, height = 15 }
```
-}
dimension : Matrix a -> Dimension
dimension aMatrix =
    case Array.length aMatrix of
        0 ->
            Dimension 0 0

        aHeight ->
            Dimension
                (Array.length <| Array.Extra.getUnsafe 0 aMatrix)
                aHeight


{-| Specialized helper for overlaying a matrix of Maybe's on top of another
matrix of Maybe's at a certain offset. Ensures that a `Nothing` in the overlay
won't mask a Just in the base layer.

This is implemented in terms of an `indexedMap`.

```
gridHelper : Int -> Int -> Maybe Int
gridHelper x y =
    if (x + y) % 2 == 0 then
        Just 1

    else
        Nothing


base : Matrix (Maybe Int)
base =
    Matrix.create 5 5 gridHelper


overlay : Matrix (Maybe Int)
overlay =
    Matrix.create 3 3 gridHelper


doOverlay : Matrix (Maybe Int)
doOverlay =
    Matrix.overlayMaybe overlay 1 1 base
```
-}
overlayMaybe : Matrix (Maybe a) -> Int -> Int -> Matrix (Maybe a) -> Matrix (Maybe a)
overlayMaybe over offsetX offsetY on =
    indexedMap
        (\x y original ->
            case get (y - offsetY) (x - offsetX) over |> Maybe.Extra.join of
                Nothing ->
                    Just original |> Maybe.Extra.join

                Just aBlock ->
                    Just aBlock
        )
        on


{-| Create an empty, zero-dimension matrix.

```
Matrix.toList Matrix.empty == []
```
-}
empty : Matrix a
empty =
    Array.empty


{-| Create a matrix with the given width and height, and initializes the values
using the initialization helper provided which turns an x y position into a
value.

```
aMatrix : Matrix Int
aMatrix =
    let
        width = 3
        height = 2
    in
        Matrix.create
            width
            height
            (\x y -> x + y * width)

aMatrix |> Matrix.toList ==
    [ [ 1, 2, 3 ]
    , [ 4, 5, 6]
    ]
```
-}
create : Int -> Int -> (Int -> Int -> a) -> Matrix a
create width height initialValue =
    let
        initRow y =
            Array.initialize width (\x -> initialValue x y)
    in
        Array.initialize height initRow


{-| Create a matrix with the given with and height, with each field initialized
as a constant value.

```
aConstantMatrix : Matrix Int
aConstantMatrix =
    Matrix.constant 3 2 1

Matrix.toList aConstantMatrix ==
    [ [ 1, 1, 1 ]
    , [ 1, 1, 1 ]
    ]
```
-}
constant : Int -> Int -> a -> Matrix a
constant width height initialValue =
    let
        row =
            Array.initialize width (always initialValue)
    in
        Array.initialize height (always row)


{-| Returns the values of a specified row as a `Maybe (List a)`. If the
rownumber is out of bounds, this returns `Nothing`, else `Just [..]`.

```
-- Building upon the example from `create`
aMatrix : Matrix Int
aMatrix =
    let
        width = 3
        height = 2
    in
        Matrix.create
            width
            height
            (\x y -> x + y * width)

aMatrix |> Matrix.toList ==
    [ [ 1, 2, 3 ]
    , [ 4, 5, 6]
    ]

-- Getting rows is rather easy
aMatrix |> Matrix.row 0 == Just [ 1, 2, 3 ]
aMatrix |> Matrix.row 2 == Nothing -- Out of bounds!
```
-}
row : Int -> Matrix a -> Maybe (List a)
row y matrix =
    Array.get y matrix
        |> Maybe.map Array.toList


{-| Return the values of a specified column as a `Maybe (List a)`. If the
columnnumber is out of bounds, this returns `Nothing`, else `Just [..]`.

```
-- Building upon the example from `create`
aMatrix : Matrix Int
aMatrix =
    let
        width = 3
        height = 2
    in
        Matrix.create
            width
            height
            (\x y -> x + y * width)

aMatrix |> Matrix.toList ==
    [ [ 1, 2, 3 ]
    , [ 4, 5, 6]
    ]

-- Getting rows is rather easy
aMatrix |> Matrix.column 0 == Just [ 1, 4 ]
aMatrix |> Matrix.column 3 == Nothing -- Out of bounds!
```
-}
column : Int -> Matrix a -> Maybe (List a)
column x matrix =
    Array.map (Array.get x) matrix
        |> Array.toList
        |> Maybe.Extra.combine


{-| Get the value at the specified coordinates in a Matrix. If the location is
out of bounds, this will yield Nothing.

```
-- Building upon the example from `create`
aMatrix : Matrix Int
aMatrix =
    let
        width = 3
        height = 2
    in
        Matrix.create
            width
            height
            (\x y -> x + y * width)

aMatrix |> Matrix.toList ==
    [ [ 1, 2, 3 ]
    , [ 4, 5, 6]
    ]


Matrix.get 0 0 aMatrix == Just 1
Matrix.get 1 1 aMatrix == Just 5
Matrix.get 0 2 aMatrix == Nothing -- Out of bounds..
```
-}
get : Int -> Int -> Matrix a -> Maybe a
get y x matrix =
    Array.get y matrix
        |> Maybe.map (Array.get x)
        |> Maybe.Extra.join


{-| Unsafely get a value from a matrix. Will crash the program on failure.
-}
unsafeGet : Int -> Int -> Matrix a -> a
unsafeGet y x matrix =
    case get y x matrix of
        Nothing ->
            Debug.crash "k thx bye"

        Just val ->
            val


{-| An indexedMap where the operator is given an x and y coordinate as well as
the current value.

```
aMatrix : Matrix Int
aMatrix =
    Matrix.create 3 2 (\x y -> x + y * 3)


aMatrix |> Matrix.toList ==
    [ [ 1, 2, 3 ]
    , [ 4, 5, 6]
    ]


aMatrix
    |> Matrix.indexedMap (\x y val -> val % 2 == 0)
    |> Matrix.toList ==
    [ [ False, True, False ]
    , [ True, False, True ]
    ]
```
-}
indexedMap : (Int -> Int -> a -> b) -> Matrix a -> Matrix b
indexedMap operator aMatrix =
    aMatrix
        |> Array.indexedMap
            (\y row ->
                Array.indexedMap
                    (\x elem -> operator x y elem)
                    row
            )


{-| Map an operator over a Matrix.

```
aMatrix : Matrix Int
aMatrix =
    Matrix.constant 3 2 1
        |>


aMatrix |> Matrix.toList ==
        [ [ 1, 1, 1 ]
        , [ 1, 1, 1 ]
        ]

-- Multiply every entry by 2
aMatrix
    |> Matrix.map ((*) 2)
    |> Matrix.toList ==
    [ [ 2, 2, 2 ]
    , [ 2, 2, 2]
    ]
```
-}
map : (a -> b) -> Matrix a -> Matrix b
map operator matrix =
    matrix
        |> Array.map (Array.map operator)


{-| Fold over the values.

```
aMatrix : Matrix Int
aMatrix =
    Matrix.constant 3 2 1


aMatrix
    |> Matrix.foldl ((::)) []
    == [ 1, 2, 3, 4, 5, 6 ]
```
-}
foldl : (a -> b -> b) -> b -> Matrix a -> b
foldl accumulator initial matrix =
    Array.foldl
        (\row acc -> Array.foldl accumulator acc row)
        initial
        matrix


{-| Turns a Matrix into a list of lists. Outer lists are rows, inner lists are
columns.

```
-- A special case, the empty matrix with dimension 0 0
Matrix.empty |> Matrix.toList == []

-- A normal matrix
Matrix.constant 3 2 1
    |> Matrix.toList ==
    [ [ 1, 1, 1 ]
    , [ 1, 1, 1 ]
    ]
```
-}
toList : Matrix a -> List (List a)
toList matrix =
    Array.toList matrix
        |> List.map Array.toList


{-| Turn a list of lists into a Matrix, provided that all sublists are of equal
length.
-}
fromList : List (List a) -> Maybe (Matrix a)
fromList aListOfList =
    Array.fromList aListOfList
        |> Array.map Array.fromList
        |> consistentOrNothing


consistentOrNothing : Matrix a -> Maybe (Matrix a)
consistentOrNothing aMatrix =
    let
        { width, height } =
            dimension aMatrix
    in
        if height == 0 then
            Just aMatrix
        else if arrayAll (\row -> Array.length row == width) aMatrix then
            Just aMatrix
        else
            Nothing


arrayAll : (a -> Bool) -> Array a -> Bool
arrayAll predicate anArray =
    anArray
        |> Array.toList
        |> List.all predicate


{-| Unsafely provides the inverse of Matrix.toList. If not all sublists are of
equal length, it crashes.
-}
unsafeFromList : List (List a) -> Matrix a
unsafeFromList aListOfLists =
    case fromList aListOfLists of
        Nothing ->
            Debug.crash "Nein"

        Just aMatrix ->
            aMatrix


reverseArray : Array a -> Array a
reverseArray anArray =
    let
        length =
            Array.length anArray

        getMatchingElement : Int -> a
        getMatchingElement x =
            Array.Extra.getUnsafe (length - x - 1) anArray
    in
        Array.initialize length getMatchingElement


{-| Horizontally flips a Matrix.
-}
flipHorizontal : Matrix a -> Matrix a
flipHorizontal =
    Array.map reverseArray


{-| Vertically flips a Matrix.
-}
flipVertical : Matrix a -> Matrix a
flipVertical =
    reverseArray


{-| Transpose a Matrix, i.e. exchanging rows for columns and columns for rows.

```
aMatrix : Matrix Int
aMatrix =
    Matrix.create 2 3 (\x y -> x + y * 2)

aMatrix |> Matrix.toList ==
    [ [ 1, 2 ]
    , [ 3, 4 ]
    , [ 5, 6 ]
    ]


Matrix.transpose aMatrix |> Matrix.toList ==
    [ [ 1, 3, 5 ]
    , [ 2, 4, 6 ]
    ]
```
-}
transpose : Matrix a -> Matrix a
transpose aMatrix =
    let
        { width, height } =
            dimension aMatrix
    in
        create height width (\x y -> unsafeGet x y aMatrix)


{-| Transform a matrix by rotating it a quarter turn, clockwise
-}
rotate : Matrix a -> Matrix a
rotate aMatrix =
    aMatrix |> transpose |> flipHorizontal

module Matrix exposing (Matrix, create, empty, map, flipHorizontal, flipVertical)

import Array exposing (Array)
import Maybe.Extra
import Array.Extra


type alias Matrix a =
    Array (Array a)


empty : Matrix a
empty =
    Array.empty


create : Int -> Int -> (Int -> Int -> a) -> Matrix a
create width height initialValue =
    let
        initRow y =
            Array.initialize width (\x -> initialValue x y)
    in
        Array.initialize height initRow


row : Int -> Matrix a -> Maybe (List a)
row y matrix =
    Array.get y matrix
        |> Maybe.map Array.toList


column : Int -> Matrix a -> Maybe (List a)
column x matrix =
    Array.map (Array.get x) matrix
        |> Array.toList
        |> Maybe.Extra.combine


get : Int -> Int -> Matrix a -> Maybe a
get y x matrix =
    Array.get y matrix
        |> Maybe.map (Array.get x)
        |> Maybe.Extra.join


map : (a -> b) -> Matrix a -> List (List b)
map operator matrix =
    Array.toList matrix
        |> List.map (\aRow -> List.map operator <| Array.toList aRow)


reverseArray : Array a -> Array a
reverseArray anArray =
    let
        length =
            Array.length anArray

        getMatchingElement : Int -> a
        getMatchingElement x =
            Array.Extra.getUnsafe (length - x) anArray
    in
        Array.initialize length getMatchingElement


flipHorizontal : Matrix a -> Matrix a
flipHorizontal =
    Array.map reverseArray


flipVertical : Matrix a -> Matrix a
flipVertical =
    reverseArray

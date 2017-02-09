module Matrix.Helpers exposing (..)

import Matrix exposing (Matrix)
import Array exposing (Array)
import Maybe.Extra
import Mouse exposing (Position)


listsToPairs : List a -> List b -> List ( a, b )
listsToPairs firstList secondList =
    List.concatMap
        (\f ->
            List.map (\s -> ( f, s )) secondList
        )
        firstList


pairToPosition : ( Int, Int ) -> Position
pairToPosition ( x, y ) =
    { x = x, y = y }


removeFullLines : Matrix (Maybe a) -> ( Matrix (Maybe a), Int )
removeFullLines matrix =
    let
        arrayAll : (z -> Bool) -> Array z -> Bool
        arrayAll op array =
            array |> Array.toList |> List.all op

        isRowDone : Array (Maybe z) -> Bool
        isRowDone row =
            arrayAll Maybe.Extra.isJust row

        handleRow : Array (Maybe z) -> ( Array (Maybe z), Int )
        handleRow array =
            if isRowDone array == True then
                ( Array.map (always Nothing) array, Array.length array )
            else
                ( array, 0 )

        handleRows : ( Matrix (Maybe z), Int ) -> ( Matrix (Maybe z), Int )
        handleRows ( matrix, initialScore ) =
            matrix
                |> Array.map handleRow
                |> Array.toList
                |> List.unzip
                |> Tuple.mapFirst Array.fromList
                |> Tuple.mapSecond List.sum
                |> Tuple.mapSecond (\score -> score + initialScore)

        handleColumns : ( Matrix (Maybe z), Int ) -> ( Matrix (Maybe z), Int )
        handleColumns ( matrix, initialScore ) =
            matrix
                |> Matrix.transpose
                |> flip (,) initialScore
                |> handleRows
                |> Tuple.mapFirst Matrix.transpose
    in
        matrix
            |> flip (,) 0
            |> handleRows
            |> handleColumns


canFitAnywhere : Matrix (Maybe a) -> Matrix (Maybe a) -> Bool
canFitAnywhere baseMatrix overlay =
    let
        baseDims : Matrix.Dimension
        baseDims =
            Matrix.dimension baseMatrix

        overlayDims : Matrix.Dimension
        overlayDims =
            Matrix.dimension overlay

        occupationPredicate : Position -> Bool
        occupationPredicate =
            isUnoccupied baseMatrix overlay
    in
        listsToPairs
            (List.range 0 <| baseDims.width - overlayDims.width)
            (List.range 0 <| baseDims.height - overlayDims.height)
            |> List.map pairToPosition
            |> List.any occupationPredicate


isUnoccupied : Matrix (Maybe a) -> Matrix (Maybe a) -> Position -> Bool
isUnoccupied baseMatrix overlay offSet =
    let
        { width, height } =
            Matrix.dimension overlay
    in
        listsToPairs
            (List.range 0 (width - 1))
            (List.range 0 (height - 1))
            |> List.all
                (\( x, y ) ->
                    (Maybe.Extra.isNothing
                        (Matrix.get y x overlay |> Maybe.Extra.join)
                    )
                        || (Maybe.Extra.isNothing
                                (Matrix.get (y + offSet.y) (x + offSet.x) baseMatrix |> Maybe.Extra.join)
                           )
                )


valueOfMatrix : Matrix (Maybe a) -> Int
valueOfMatrix aMatrix =
    Matrix.foldl
        (\val ->
            (+) <|
                case val of
                    Nothing ->
                        0

                    Just _ ->
                        1
        )
        0
        aMatrix

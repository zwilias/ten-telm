module Block
    exposing
        ( BlockType
        , Block
        , possibleTransformations
        , generateCandidate
        , blockTypeToClass
        , blockToMatrix
        , Transformation
        )

import Matrix exposing (Matrix)
import Random.Pcg as Random
import MouseMovement exposing (Drag)
import Array
import Maybe.Extra
import Styles


type alias Block =
    { blockType : BlockType
    , drag : Maybe Drag
    , seed : Random.Seed
    , transformations : Int
    }


type Transformation
    = HorizontalFlip
    | VerticalFlip


type BlockType
    = BigL
    | SmallL
    | BigBox
    | SmallBox
    | Dot
    | Dash
    | LongDash
    | LongerDash
    | LongestDash


createBlock : BlockType -> Random.Seed -> Block
createBlock blockType seedRandom =
    Block blockType Nothing seedRandom 0


possibleTransformations : BlockType -> Int
possibleTransformations blockType =
    case blockType of
        Dot ->
            0

        SmallL ->
            3

        BigL ->
            3

        SmallBox ->
            0

        BigBox ->
            0

        Dash ->
            1

        LongDash ->
            1

        LongerDash ->
            1

        LongestDash ->
            1


generateCandidate : Random.Seed -> ( Block, Random.Seed )
generateCandidate seed =
    let
        possibleTypes : List BlockType
        possibleTypes =
            [ BigL
            , SmallL
            , BigBox
            , SmallBox
            , Dot
            , Dash
            , LongDash
            , LongerDash
            , LongestDash
            ]

        intGen : Random.Generator Int
        intGen =
            Random.int 0 <| List.length possibleTypes - 1

        ( randomNumber, nextSeed ) =
            Random.step intGen seed
    in
        let
            blockType =
                case randomNumber of
                    0 ->
                        BigL

                    1 ->
                        SmallL

                    2 ->
                        BigBox

                    3 ->
                        SmallBox

                    4 ->
                        Dot

                    5 ->
                        Dash

                    6 ->
                        LongDash

                    7 ->
                        LongerDash

                    8 ->
                        LongestDash

                    _ ->
                        Debug.crash "Nein."
        in
            createBlock blockType seed
                |> (\block -> ( block, nextSeed ))
                |> addTransformations


addTransformations : ( Block, Random.Seed ) -> ( Block, Random.Seed )
addTransformations ( block, seedRandom ) =
    let
        numberAllowed : Int
        numberAllowed =
            possibleTransformations block.blockType

        intGen : Random.Generator Int
        intGen =
            Random.int 0 numberAllowed

        ( transformations, newSeed ) =
            case numberAllowed of
                0 ->
                    ( 0, seedRandom )

                _ ->
                    Random.step intGen seedRandom
    in
        ( { block
            | transformations = transformations
          }
        , newSeed
        )


shapeToMatrixInitializer :
    BlockType
    -> List (List Bool)
    -> Int
    -> Int
    -> Maybe BlockType
shapeToMatrixInitializer blockType shapeAsLists x y =
    let
        shape =
            shapeAsLists
                |> List.map Array.fromList
                |> Array.fromList
    in
        Array.get y shape
            |> Maybe.map (\row -> Array.get x row)
            |> Maybe.Extra.join
            |> Maybe.map
                (\x ->
                    if x == True then
                        Just blockType
                    else
                        Nothing
                )
            |> Maybe.Extra.join


blockToMatrix : Block -> Matrix (Maybe BlockType)
blockToMatrix block =
    let
        myMatrix : Matrix (Maybe BlockType)
        myMatrix =
            case block.blockType of
                BigL ->
                    Matrix.create
                        3
                        3
                        (shapeToMatrixInitializer
                            BigL
                            [ [ True, True, True ]
                            , [ True, False, False ]
                            , [ True, False, False ]
                            ]
                        )

                SmallL ->
                    Matrix.create
                        2
                        2
                        (shapeToMatrixInitializer
                            SmallL
                            [ [ True, False ]
                            , [ True, True ]
                            ]
                        )

                BigBox ->
                    Matrix.constant 3 3 <| Just BigBox

                SmallBox ->
                    Matrix.constant 2 2 <| Just SmallBox

                Dot ->
                    Matrix.constant 1 1 <| Just Dot

                Dash ->
                    Matrix.constant 2 1 <| Just Dash

                LongDash ->
                    Matrix.constant 3 1 <| Just LongDash

                LongerDash ->
                    Matrix.constant 4 1 <| Just LongerDash

                LongestDash ->
                    Matrix.constant 5 1 <| Just LongestDash
    in
        applyTransformation block.transformations myMatrix


applyTransformation : Int -> Matrix a -> Matrix a
applyTransformation times matrix =
    case times of
        0 ->
            matrix

        _ ->
            matrix
                |> Matrix.rotate
                |> applyTransformation (times - 1)


blockTypeToClass : Maybe BlockType -> Styles.CssClass
blockTypeToClass blockType =
    case blockType of
        Nothing ->
            Styles.EmptyBlock

        Just aType ->
            case aType of
                Dot ->
                    Styles.Dot

                SmallBox ->
                    Styles.SmallBox

                BigBox ->
                    Styles.BigBox

                SmallL ->
                    Styles.SmallL

                BigL ->
                    Styles.BigL

                Dash ->
                    Styles.Dash

                LongDash ->
                    Styles.LongDash

                LongerDash ->
                    Styles.LongerDash

                LongestDash ->
                    Styles.LongestDash

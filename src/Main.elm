module Main exposing (main)

{-| The actual app.

@docs main
-}

import Html exposing (Html, text, body, div)
import Html.CssHelpers
import Html.Events exposing (on)
import Html.Attributes exposing (style)
import Html.Lazy exposing (lazy)
import Rocket exposing ((=>))
import Matrix exposing (Matrix)
import Random.Pcg as Random
import Styles
import Maybe.Extra
import Array exposing (Array)
import Mouse exposing (Position)
import Json.Decode as Decode
import Debug
import DOM


{-| main app.
-}
main : Program Flags Model Msg
main =
    Html.programWithFlags
        { init = init >> Rocket.batchInit
        , view = view
        , update = update >> Rocket.batchUpdate
        , subscriptions = subscriptions
        }



-- Config


( fieldWidth, fieldHeight, candidates ) =
    ( 10, 10, 3 )



-- Debug helper


isDebug : Bool
isDebug =
    True


log : String -> a -> a
log string logMe =
    case isDebug of
        True ->
            Debug.log string logMe

        False ->
            logMe



-- Model


type alias Flags =
    Int


type alias Model =
    { score : Int
    , field : Field
    , seed : Random.Seed
    , next : List (Maybe Block)
    , phase : Phase
    }


type Phase
    = Playing
    | GameOver


init : Flags -> ( Model, List (Cmd Msg) )
init flags =
    { score = 0
    , field = emptyField fieldWidth fieldHeight
    , seed = Random.initialSeed flags
    , next = []
    , phase = Playing
    }
        |> generateCandidates
        => []


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


generateCandidates : Model -> Model
generateCandidates model =
    let
        ( newCandidates, newSeed ) =
            List.range 1 candidates
                |> List.foldl
                    (\_ ( candidateList, seed ) ->
                        let
                            ( newCandidate, newSeed ) =
                                generateCandidate seed
                        in
                            ( Just newCandidate :: candidateList, newSeed )
                    )
                    ( [], model.seed )
    in
        case log "next" <| List.filter Maybe.Extra.isJust model.next of
            [] ->
                { model
                    | seed = newSeed
                    , next = newCandidates
                }

            _ ->
                model


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


type alias Block =
    { blockType : BlockType
    , drag : Maybe Drag
    , seed : Random.Seed
    , transformations : Int
    }


createBlock : BlockType -> Random.Seed -> Block
createBlock blockType seedRandom =
    Block blockType Nothing seedRandom 0


type Transformation
    = HorizontalFlip
    | VerticalFlip


type alias Drag =
    { start : Position
    , current : Position
    , topLeftPos : Position
    }


type alias FieldLocation =
    { position : Position, offSet : Position }


type alias Field =
    { blocks : Matrix (Maybe BlockType)
    , mousePosition : Maybe FieldLocation
    }


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


getFieldPosition : FieldLocation -> Position
getFieldPosition { position, offSet } =
    log "fieldpos" <| getDelta position offSet


getTopLeft : Drag -> Position
getTopLeft { current, topLeftPos, start } =
    getDelta start current
        |> getDelta topLeftPos
        |> log "topleft"


getDelta : Position -> Position -> Position
getDelta from to =
    Position
        (from.x - to.x)
        (from.y - to.y)


emptyField : Int -> Int -> Field
emptyField width height =
    Field (Matrix.constant width height Nothing) Nothing



-- Update


type DragMsg
    = DragStart Block Position Position
    | DragAt Block Position
    | DragEnd Block Position


type Msg
    = DragAction DragMsg
    | MouseEnterField Position Position
    | MouseLeaveField


update : Msg -> Model -> ( Model, List (Cmd Msg) )
update msg model =
    case msg of
        DragAction dragMsg ->
            handleDragAction dragMsg model

        MouseEnterField offSet position ->
            let
                setPosition : Position -> Field -> Field
                setPosition position field =
                    { field | mousePosition = Just { offSet = offSet, position = position } }
            in
                { model
                    | field = setPosition position model.field
                }
                    => []

        MouseLeaveField ->
            let
                unsetPosition : Field -> Field
                unsetPosition field =
                    { field
                        | mousePosition = Nothing
                    }
            in
                { model
                    | field = unsetPosition model.field
                }
                    => []


handleDragAction : DragMsg -> Model -> ( Model, List (Cmd Msg) )
handleDragAction dragMsg model =
    case dragMsg of
        DragStart block topLeftPos position ->
            let
                newBlocks =
                    replaceBlock
                        block
                        (Just
                            { block
                                | drag = Just <| Drag position position topLeftPos
                            }
                            |> log "registerest drag start"
                        )
                        model.next
            in
                { model
                    | next = newBlocks
                }
                    => []

        DragAt block position ->
            let
                updateDrag : Maybe Drag -> Maybe Drag
                updateDrag =
                    Maybe.map (\drag -> { drag | current = position })

                newBlocks =
                    replaceBlock
                        block
                        (Just { block | drag = updateDrag block.drag })
                        model.next
            in
                { model
                    | next = newBlocks
                }
                    => []

        DragEnd block position ->
            let
                blockMatrix : Matrix (Maybe BlockType)
                blockMatrix =
                    blockToMatrix block

                ( newBlock, scoreD, newFieldBlocks ) =
                    case dropLocation block model.field of
                        Nothing ->
                            ( (Just { block | drag = Nothing }), 0, model.field.blocks )

                        Just location ->
                            let
                                ( newFieldBlocks, scoreD ) =
                                    (Matrix.overlayMaybe blockMatrix location.x location.y model.field.blocks)
                                        |> removeFullLines
                            in
                                ( Nothing
                                , scoreD + valueOfMatrix blockMatrix
                                , newFieldBlocks
                                )

                newBlocks : List (Maybe Block) -> List (Maybe Block)
                newBlocks oldBlocks =
                    replaceBlock
                        block
                        newBlock
                        oldBlocks

                newField : Field -> Field
                newField field =
                    { field | blocks = newFieldBlocks }
            in
                { model
                    | next = newBlocks model.next
                    , field = newField model.field
                    , score = model.score + scoreD
                }
                    |> generateCandidates
                    |> updateGamePhase
                    => []


removeFullLines : Matrix (Maybe a) -> ( Matrix (Maybe a), Int )
removeFullLines matrix =
    let
        arrayAll : (z -> Bool) -> Array z -> Bool
        arrayAll op array =
            array |> Array.toList |> List.all op

        isRowDone : Array (Maybe z) -> Bool
        isRowDone row =
            log "row done" <| arrayAll Maybe.Extra.isJust row

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


dropLocation : Block -> Field -> Maybe Position
dropLocation block field =
    let
        getOffsetLocation :
            Maybe Drag
            -> Maybe FieldLocation
            -> Maybe Position
        getOffsetLocation drag fieldLocation =
            let
                dragPosition =
                    Maybe.map getTopLeft drag

                fieldPosition =
                    Maybe.map getFieldPosition fieldLocation
            in
                Maybe.map2 getDelta dragPosition fieldPosition

        localizedDelta =
            getOffsetLocation
                block.drag
                field.mousePosition

        blockMatrix : Matrix (Maybe BlockType)
        blockMatrix =
            blockToMatrix block

        size : Matrix.Dimension
        size =
            Matrix.dimension blockMatrix

        nearestLocation =
            Maybe.map positionToNearestLocation localizedDelta
                |> log "nearest location"
    in
        Maybe.Extra.filter
            (positionInBounds
                (fieldWidth - size.width)
                (fieldHeight - size.height)
            )
            nearestLocation
            |> log "is in bounds"
            |> Maybe.Extra.filter
                (\location ->
                    isUnoccupied field.blocks blockMatrix location
                )
            |> log "is unoccupied"


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


updateGamePhase : Model -> Model
updateGamePhase model =
    let
        hasPossibleMoves : Bool
        hasPossibleMoves =
            model.next
                |> Maybe.Extra.values
                |> List.map blockToMatrix
                |> List.any (canFitAnywhere model.field.blocks)
    in
        case hasPossibleMoves of
            True ->
                log "some moves left" model

            False ->
                { model | phase = GameOver }
                    |> log "no moves left, game over!"


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
                |> log "dims"
    in
        listsToPairs
            (List.range 0 (width - 1))
            (List.range 0 (height - 1))
            |> log "checking ranges..."
            |> List.all
                (\( x, y ) ->
                    (Maybe.Extra.isNothing
                        (Matrix.get y x overlay |> Maybe.Extra.join)
                    )
                        || (Maybe.Extra.isNothing
                                (Matrix.get (y + offSet.y) (x + offSet.x) baseMatrix |> Maybe.Extra.join)
                           )
                        |> log ("isNothing at " ++ toString x ++ ", " ++ toString y)
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


inBounds : Int -> Int -> Int -> Bool
inBounds lower upper number =
    number >= lower && number <= upper


positionInBounds : Int -> Int -> Position -> Bool
positionInBounds width height { x, y } =
    inBounds 0 width x && inBounds 0 height y


nearestMultiplier : Int -> Int -> Int
nearestMultiplier multiple number =
    (number + multiple // 2) // multiple


positionToNearestLocation : Position -> Position
positionToNearestLocation position =
    { position
        | x = nearestMultiplier 44 position.x
        , y = nearestMultiplier 44 position.y
    }


replaceBlock : Block -> Maybe Block -> List (Maybe Block) -> List (Maybe Block)
replaceBlock oldBlock newBlock blockList =
    let
        replacer : Maybe Block -> Maybe Block
        replacer maybeBlock =
            case maybeBlock of
                Nothing ->
                    Nothing

                Just possibleBlock ->
                    if possibleBlock == oldBlock then
                        newBlock
                    else
                        Just possibleBlock
    in
        blockList
            |> List.map replacer



-- Subscriptions


findFirst : (a -> Bool) -> List (Maybe a) -> Maybe a
findFirst predicate aList =
    case aList of
        [] ->
            Nothing

        head :: tail ->
            case head of
                Nothing ->
                    findFirst predicate tail

                Just something ->
                    if predicate something then
                        Just something
                    else
                        findFirst predicate tail


subscriptions : Model -> Sub Msg
subscriptions model =
    case findFirst (\block -> Maybe.Extra.isJust block.drag) model.next of
        Nothing ->
            Sub.none

        Just block ->
            [ DragAt block |> Mouse.moves |> Sub.map DragAction
            , DragEnd block |> Mouse.ups |> Sub.map DragAction
            ]
                |> Sub.batch



-- View


{ id, class, classList } =
    Html.CssHelpers.withNamespace "tenten"


view : Model -> Html Msg
view model =
    div [ class [ Styles.Wrapper ] ]
        [ div [ class [ Styles.Score ] ] [ text <| toString model.score ]
        , renderField model.field
        , renderNext model.next
        , renderPhase model.phase
        ]


renderPhase : Phase -> Html Msg
renderPhase phase =
    case phase of
        GameOver ->
            renderGameOver

        _ ->
            text ""


renderGameOver : Html Msg
renderGameOver =
    div [ class [ Styles.GameOver ] ] [ text "Game Over!" ]


renderNext : List (Maybe Block) -> Html Msg
renderNext blockList =
    div [ class [ Styles.CandidateList ] ] <| List.map renderCandidate blockList


renderCandidate : Maybe Block -> Html Msg
renderCandidate block =
    case block of
        Nothing ->
            div [] []

        Just aBlock ->
            let
                classes =
                    case aBlock.drag of
                        Nothing ->
                            [ Styles.Candidate ]

                        Just _ ->
                            [ Styles.Candidate, Styles.Dragging ]
            in
                div
                    [ class classes
                    , on "mousedown" <| startDrag aBlock
                    , style [ dragPosition aBlock, dragTransition aBlock ]
                    ]
                    (Matrix.map renderFieldBlock (blockToMatrix aBlock)
                        |> Matrix.toList
                        |> List.map renderRow
                    )


dragTransition : Block -> ( String, String )
dragTransition block =
    case block.drag of
        Nothing ->
            ( "transition", "transform 0.2s ease-in-out" )

        Just _ ->
            ( "transition", "none" )


dragPosition : Block -> ( String, String )
dragPosition block =
    case block.drag of
        Nothing ->
            ( "", "" )

        Just { start, current } ->
            "translate("
                ++ (toString <| current.x - start.x)
                ++ "px"
                ++ ", "
                ++ (toString <| current.y - start.y)
                ++ "px"
                ++ ")"
                |> (,) "transform"


renderField : Field -> Html Msg
renderField field =
    div
        [ class [ Styles.Field ]
        , on "mouseenter" registerMouseEnter
        , on "mouseleave" registerMouseLeave
        ]
        (Matrix.map (lazy renderFieldBlock) field.blocks
            |> Matrix.toList
            |> List.map (lazy renderRow)
        )


renderRow : List (Html Msg) -> Html Msg
renderRow fieldRow =
    div [ class [ Styles.FieldRow ] ] fieldRow


renderFieldBlock : Maybe BlockType -> Html Msg
renderFieldBlock fieldBlock =
    let
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
    in
        div [ class [ Styles.FieldBlock, blockTypeToClass fieldBlock ] ] []



-- Event Decoders


offsetPositionDecoder : Decode.Decoder Position
offsetPositionDecoder =
    Decode.map2 Position
        (Decode.field "offsetX" Decode.int)
        (Decode.field "offsetY" Decode.int)


registerMouseLeave : Decode.Decoder Msg
registerMouseLeave =
    Decode.succeed MouseLeaveField


registerMouseEnter : Decode.Decoder Msg
registerMouseEnter =
    Decode.map2 MouseEnterField
        offsetPositionDecoder
        Mouse.position


startDrag : Block -> Decode.Decoder Msg
startDrag block =
    Decode.map2 (DragStart block)
        topLeft
        Mouse.position
        |> Decode.map DragAction


topLeft : Decode.Decoder Position
topLeft =
    Decode.map2 (,)
        DOM.offsetLeft
        DOM.offsetTop
        |> currentTarget
        |> Decode.andThen
            (\( x, y ) ->
                Decode.succeed <| Position (round x) (round y)
            )


currentTarget : Decode.Decoder a -> Decode.Decoder a
currentTarget decoder =
    Decode.field "currentTarget" decoder

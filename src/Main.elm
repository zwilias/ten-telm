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
import Mouse exposing (Position)
import Json.Decode as Decode
import Debug
import DOM
import Block
    exposing
        ( BlockType
        , Block
        , blockTypeToClass
        , blockToMatrix
        , generateCandidate
        )
import MouseMovement exposing (Drag)
import Matrix.Helpers exposing (..)


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


type alias FieldLocation =
    { position : Position, offSet : Position }


type alias Field =
    { blocks : Matrix (Maybe BlockType)
    , mousePosition : Maybe FieldLocation
    }


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

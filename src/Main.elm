module Main exposing (main)

import Html exposing (Html, text, body, div)
import Html.CssHelpers
import Html.Events exposing (on)
import Html.Attributes exposing (style)
import Html.Lazy exposing (lazy)
import Rocket exposing ((=>))
import Matrix exposing (Matrix)
import Random
import Styles
import Maybe.Extra
import Array exposing (Array)
import Mouse exposing (Position)
import Json.Decode as Decode
import Debug


main : Program Never Model Msg
main =
    Html.program
        { init = init
        , view = view
        , update = update >> Rocket.batchUpdate
        , subscriptions = subscriptions
        }



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
    | Pyramid


type alias Block =
    { blockType : BlockType
    , drag : Maybe Drag
    }


type alias Drag =
    { start : Position
    , current : Position
    , offSet : Position
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

        Pyramid ->
            Matrix.create 3
                2
                (shapeToMatrixInitializer
                    Pyramid
                    [ [ False, True, False ]
                    , [ True, True, True ]
                    ]
                )

        LongestDash ->
            Matrix.create 5
                1
                (shapeToMatrixInitializer
                    LongestDash
                    [ [ True, True, True, True, True ] ]
                )

        _ ->
            Matrix.empty


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
getTopLeft { current, offSet, start } =
    getDelta current offSet
        |> log "topleft"


getDelta : Position -> Position -> Position
getDelta from to =
    Position
        (from.x - to.x)
        (from.y - to.y)


emptyField : Int -> Int -> Field
emptyField width height =
    Field (Matrix.create width height (always <| always Nothing)) Nothing


type alias Model =
    { score : Int
    , field : Field
    , seed : Random.Seed
    , next : List (Maybe Block)
    }


init : ( Model, Cmd Msg )
init =
    ( { score = 0
      , field = emptyField 10 10
      , seed = Random.initialSeed 227852860
      , next =
            [ Just <| Block BigL Nothing
            , Just <| Block Pyramid Nothing
            , Just <| Block LongestDash Nothing
            ]
      }
    , Cmd.none
    )



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
    case log "message" msg of
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
        DragStart block offSet position ->
            let
                newBlocks =
                    replaceBlock
                        block
                        { block
                            | drag = Just <| Drag position position offSet
                        }
                        model.next
            in
                { model
                    | next = newBlocks
                }
                    => []

        DragAt block position ->
            let
                updateDrag : Maybe Drag -> Maybe Drag
                updateDrag maybeDrag =
                    case maybeDrag of
                        Nothing ->
                            Nothing

                        Just drag ->
                            Just { drag | current = position }

                newBlocks =
                    replaceBlock
                        block
                        { block | drag = updateDrag block.drag }
                        model.next
            in
                { model
                    | next = newBlocks
                }
                    => []

        DragEnd block position ->
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

                _ =
                    log "localized delta = " <|
                        getOffsetLocation
                            block.drag
                            model.field.mousePosition

                newBlocks =
                    replaceBlock
                        block
                        { block | drag = Nothing }
                        model.next
            in
                { model | next = newBlocks } => []


replaceBlock : Block -> Block -> List (Maybe Block) -> List (Maybe Block)
replaceBlock oldBlock newBlock blockList =
    let
        replacer : Maybe Block -> Maybe Block
        replacer maybeBlock =
            case maybeBlock of
                Nothing ->
                    Nothing

                Just possibleBlock ->
                    if possibleBlock == oldBlock then
                        Just newBlock
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


view : Model -> Html Msg
view model =
    div []
        [ text "Score: "
        , text <| toString model.score
        , renderField model.field
        , renderNext model.next
        ]


{ id, class, classList } =
    Html.CssHelpers.withNamespace "tenten"


renderNext : List (Maybe Block) -> Html Msg
renderNext blockList =
    div [] <| List.map renderCandidate blockList


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

                _ ->
                    Styles.FilledBlock
    in
        div [ class [ Styles.FieldBlock, blockTypeToClass fieldBlock ] ] []


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
        offsetPositionDecoder
        Mouse.position
        |> Decode.map DragAction

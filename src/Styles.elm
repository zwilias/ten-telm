module Styles exposing (css, CssClass(..))

import Css exposing (..)
import Css.Namespace exposing (namespace)


type CssClass
    = Field
    | FieldRow
    | FieldBlock
    | EmptyBlock
    | FilledBlock
    | Candidate
    | Dragging


css : Stylesheet
css =
    (stylesheet << namespace "tenten")
        [ class FieldRow
            [ (displayFlex) ]
        , class FieldBlock
            [ borderRadius (px 5)
            , width (px 40)
            , height (px 40)
            , margin (px 1)
            ]
        , class Field
            [ margin (px 10)
            , descendants
                [ class EmptyBlock
                    [ backgroundColor (hex "CFCFCF")
                    , border3 (px 1) solid (hex "CFCFCF")
                    ]
                ]
            ]
        , class FilledBlock
            [ backgroundColor (hex "FF8271")
            , border3 (px 1) solid (hex "FF8271")
            ]
        , class Candidate
            [ property "transition" "transform 0.2s ease-in-out"
            , transform (scale 0.5)
            , display inlineBlock
            , cursor move
            , hover
                [ transform (scale 1)
                ]
            ]
        , class Dragging
            [ property "pointer-events" "none" ]
        ]

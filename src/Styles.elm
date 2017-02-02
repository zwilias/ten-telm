module Styles exposing (css, CssClass(..))

{-| Define all the styling, using rtfeldman's elm-css.

@docs css, CssClass
-}

import Css exposing (..)
import Css.Namespace exposing (namespace)


{-| Enumerate all the defined css classes, so they can be referenced
type-safely.
-}
type CssClass
    = Field
    | FieldRow
    | FieldBlock
    | EmptyBlock
    | FilledBlock
    | Candidate
    | Dragging
    | BigL
    | SmallL
    | BigBox
    | SmallBox
    | Dot
    | Dash
    | LongDash
    | LongerDash
    | LongestDash
    | Wrapper
    | Score
    | CandidateList
    | GameOver



-- #FFC658


{-| Generate the stylesheet.
-}
css : Stylesheet
css =
    (stylesheet << namespace "tenten")
        [ class FieldRow
            [ (displayFlex) ]
        , class Score
            [ fontSize (em 2)
            , textAlign center
            , fontFamilies [ "Luckiest Guy" ]
            ]
        , class Wrapper
            [ margin2 (px 0) auto
            , marginTop (px 20)
            , width (px 440)
            ]
        , class GameOver
            [ displayFlex
            , alignItems center
            , justifyContent center
            , position absolute
            , width (pct 100)
            , height (pct 100)
            , top (px 0)
            , left (px 0)
            , fontFamilies [ "Luckiest Guy" ]
            , fontSize (em 3)
            , backgroundColor (rgba 30 30 30 0.3)
            ]
        , class CandidateList
            [ displayFlex
            , justifyContent spaceBetween
            ]
        , class FieldBlock
            [ borderRadius (px 5)
            , width (px 40)
            , height (px 40)
            , border3 (px 1) solid transparent
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
        , blockGen FilledBlock "333333"
        , blockGen BigL "FFC658"
        , blockGen SmallL "50B5D5"
        , blockGen Dot "5D6EB9"
        , blockGen BigBox "B3FE6A"
        , blockGen SmallBox "E47D50"
        , blockGen Dash "98DC55"
        , blockGen LongDash "7E8ED5"
        , blockGen LongerDash "448FC4"
        , blockGen LongestDash "ED954A"
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


blockGen : CssClass -> String -> Css.Snippet
blockGen className color =
    class className
        [ backgroundColor (hex color)
        , border3 (px 1) solid (hex color)
        ]

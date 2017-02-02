module MatrixTests exposing (all)

import Test exposing (..)
import Expect
import Matrix exposing (..)
import Fuzz exposing (int, intRange)
import Array


all : Test
all =
    describe "Matrix tests"
        [ describe "empty"
            [ test "creates empty Matrix" <|
                \() ->
                    Expect.equal
                        Array.empty
                        Matrix.empty
            ]
        , describe "create"
            [ fuzz int "create matrix of defined size" <|
                \value ->
                    Expect.equal
                        (Array.initialize 3 (always 0)
                            |> Array.map
                                (\row -> Array.initialize 5 (always value))
                        )
                        (Matrix.create 5 3 (always << always value))
            , fuzz int "Empty when dimensions are zero" <|
                \value ->
                    Expect.equal
                        Matrix.empty
                        (Matrix.create 0 0 (always << always value))
            ]
        , describe "dimensions"
            [ test "empty is empty" <|
                \() ->
                    let
                        expected =
                            Dimension 0 0

                        actual =
                            Matrix.dimension Matrix.empty
                    in
                        Expect.equal
                            expected
                            actual
            , fuzz2 (intRange 1 50) (intRange 1 50) "with dimensions" <|
                \width height ->
                    let
                        expected =
                            Dimension width height

                        input =
                            Matrix.create width height (always << always 0)
                    in
                        Expect.equal
                            expected
                            (Matrix.dimension input)
            ]
        ]

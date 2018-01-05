module ArrayZipper exposing (..)

import Array exposing (Array)
import Expect
import Fuzz exposing (int, list)
import Test exposing (..)
import Zipper.ArrayZipper
    exposing
        ( after
        , before
        , current
        , fromArray
        , last
        , map
        , mapAfter
        , mapBefore
        , next
        , singleton
        , toArray
        )


expectJustEqual : a -> Maybe a -> Expect.Expectation
expectJustEqual a =
    Expect.equal (Just a)


suite : Test
suite =
    describe "ArrayZipper tests"
        [ describe "singleton"
            [ fuzz int "creates a ArrayZipper pointing to the passed element" <|
                \fuzzInt ->
                    singleton fuzzInt
                        |> current
                        |> Expect.equal fuzzInt
            ]
        , describe "fromArray"
            [ test "returns Nothing if array is empty" <|
                \_ ->
                    fromArray Array.empty
                        |> Expect.equal Nothing
            , test "returns Just ArrayZipper pointing to the first element" <|
                \_ ->
                    fromArray (Array.fromList [ 1 ])
                        |> Maybe.withDefault (singleton -1)
                        |> current
                        |> Expect.equal 1
            ]
        , describe "after"
            [ test "returns an empty array when ArrayZipper has only one item" <|
                \_ ->
                    singleton 1
                        |> after
                        |> Expect.equal Array.empty
            , test "returns a array with items ArrayZipper has more than one item" <|
                \_ ->
                    fromArray (Array.fromList [ 1, 2, 3 ])
                        |> Maybe.withDefault (singleton 1)
                        |> after
                        |> Expect.equal (Array.fromList [ 2, 3 ])
            ]
        , describe "before"
            [ test "returns an empty array when ArrayZipper has only one item" <|
                \_ ->
                    singleton 1
                        |> before
                        |> Expect.equal Array.empty
            ]
        , describe "toArray"
            [ test "returns the array items inside of ArrayZipper created" <|
                \_ ->
                    fromArray (Array.fromList [ 1, 2, 3, 4 ])
                        |> Maybe.withDefault (singleton 1)
                        |> toArray
                        |> Expect.equal (Array.fromList [ 1, 2, 3, 4 ])
            ]
        , describe "map"
            [ test "maps every item with the passed fn" <|
                \_ ->
                    fromArray (Array.fromList [ 1, 2, 3, 4, 5 ])
                        |> Maybe.withDefault (singleton 1)
                        |> map ((+) 1)
                        |> toArray
                        |> Expect.equal (Array.fromList [ 2, 3, 4, 5, 6 ])
            ]
        , describe "mapBefore"
            [ test "maps items before" <|
                \_ ->
                    fromArray (Array.fromList [ 1, 2, 3, 4, 5 ])
                        |> Maybe.map last
                        |> Maybe.map (mapBefore (Array.filter ((==) 2)))
                        |> Maybe.map toArray
                        |> expectJustEqual (Array.fromList [ 2, 5 ])
            ]
        , describe "mapAfter"
            [ test "maps items after" <|
                \_ ->
                    fromArray (Array.fromList [ 1, 2, 3, 4, 5 ])
                        |> Maybe.map (mapAfter (Array.filter ((==) 2)))
                        |> Maybe.map toArray
                        |> expectJustEqual (Array.fromList [ 1, 2 ])
            ]
        , describe "next"
            [ test "returns a Just ListZipper with the next item selected" <|
                \_ ->
                    fromArray (Array.fromList [ 1, 2, 3, 4, 5 ])
                        |> Maybe.andThen next
                        |> Maybe.map current
                        |> Expect.equal (Just 2)
            , test "returns a Nothing if the selected is the last" <|
                \_ ->
                    singleton 1
                        |> next
                        |> Expect.equal Nothing
            , test "does not change the items, only selected one" <|
                \_ ->
                    fromArray (Array.fromList [ 1, 2, 3, 4, 5 ])
                        |> Maybe.andThen next
                        |> Maybe.map toArray
                        |> Expect.equal (Just (Array.fromList [ 1, 2, 3, 4, 5 ]))
            ]
        , describe "last"
            [ test "returns a Just ListZipper with the last item selected" <|
                \_ ->
                    fromArray (Array.fromList [ 1, 2, 3, 4, 5 ])
                        |> Maybe.andThen next
                        |> Maybe.andThen next
                        |> Maybe.map last
                        |> Maybe.map current
                        |> Expect.equal (Just 5)
            , test "does not change the items, only selected one" <|
                \_ ->
                    fromArray (Array.fromList [ 1, 2, 3, 4, 5 ])
                        |> Maybe.map last
                        |> Maybe.map toArray
                        |> Expect.equal (Just (Array.fromList [ 1, 2, 3, 4, 5 ]))
            , test "does not change the items, only selected one, even after moving around" <|
                \_ ->
                    fromArray (Array.fromList [ 1, 2, 3, 4, 5 ])
                        |> Maybe.andThen next
                        |> Maybe.andThen next
                        |> Maybe.map last
                        |> Maybe.map toArray
                        |> Expect.equal (Just (Array.fromList [ 1, 2, 3, 4, 5 ]))
            ]
        ]

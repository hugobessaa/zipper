module ListZipper exposing (..)

import Expect
import Fuzz exposing (int, list)
import Test exposing (..)
import Zipper.ListZipper
    exposing
        ( after
        , before
        , current
        , find
        , first
        , fromList
        , last
        , map
        , mapAfter
        , mapBefore
        , mapCurrent
        , next
        , previous
        , singleton
        , toList
        )


expectJustEqual : a -> Maybe a -> Expect.Expectation
expectJustEqual a =
    Expect.equal (Just a)


suite : Test
suite =
    describe "ListZipper tests"
        [ describe "singleton"
            [ fuzz int "creates a ListZipper pointing to the passed element" <|
                \fuzzInt ->
                    singleton fuzzInt
                        |> current
                        |> Expect.equal fuzzInt
            ]
        , describe "fromList"
            [ test "returns Nothing if list is empty" <|
                \_ ->
                    fromList []
                        |> Expect.equal Nothing
            , test "returns Just ListZipper pointing to the first element" <|
                \_ ->
                    fromList [ 1 ]
                        |> Maybe.withDefault (singleton -1)
                        |> current
                        |> Expect.equal 1
            ]
        , describe "after"
            [ test "returns an empty list when ListZipper has only one item" <|
                \_ ->
                    singleton 1
                        |> after
                        |> Expect.equal []
            , test "returns a list with items ListZipper has more than one item" <|
                \_ ->
                    fromList [ 1, 2, 3 ]
                        |> Maybe.withDefault (singleton 1)
                        |> after
                        |> Expect.equal [ 2, 3 ]
            ]
        , describe "before"
            [ test "returns an empty list when ListZipper has only one item" <|
                \_ ->
                    singleton 1
                        |> before
                        |> Expect.equal []
            ]
        , describe "toList"
            [ test "returns the list items inside of ListZipper created" <|
                \_ ->
                    fromList [ 1, 2, 3, 4 ]
                        |> Maybe.withDefault (singleton 1)
                        |> toList
                        |> Expect.equal [ 1, 2, 3, 4 ]
            ]
        , describe "map"
            [ test "maps every item with the passed fn" <|
                \_ ->
                    fromList [ 1, 2, 3, 4, 5 ]
                        |> Maybe.withDefault (singleton 1)
                        |> map ((+) 1)
                        |> toList
                        |> Expect.equal [ 2, 3, 4, 5, 6 ]
            ]
        , describe "mapBefore"
            [ test "maps items before" <|
                \_ ->
                    fromList [ 1, 2, 3, 4, 5 ]
                        |> Maybe.map last
                        |> Maybe.map (mapBefore (List.filter ((==) 2)))
                        |> Maybe.map toList
                        |> expectJustEqual [ 2, 5 ]
            ]
        , describe "mapAfter"
            [ test "maps items after" <|
                \_ ->
                    fromList [ 1, 2, 3, 4, 5 ]
                        |> Maybe.map (mapAfter (List.filter ((==) 2)))
                        |> Maybe.map toList
                        |> expectJustEqual [ 1, 2 ]
            ]
        , describe "mapCurrent"
            [ test "maps the current element" <|
                \_ ->
                    fromList [ 1, 2, 3, 4, 5 ]
                        |> Maybe.andThen next
                        |> Maybe.andThen next
                        |> Maybe.withDefault (singleton 1)
                        |> mapCurrent ((+) 1)
                        |> toList
                        |> Expect.equal [ 1, 2, 4, 4, 5 ]
            ]
        , describe "next"
            [ test "returns a Just ListZipper with the next item selected" <|
                \_ ->
                    fromList [ 1, 2, 3, 4, 5 ]
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
                    fromList [ 1, 2, 3, 4, 5 ]
                        |> Maybe.andThen next
                        |> Maybe.map toList
                        |> Expect.equal (Just [ 1, 2, 3, 4, 5 ])
            ]
        , describe "previous"
            [ test "returns a Just ListZipper with the previous item selected" <|
                \_ ->
                    fromList [ 1, 2, 3, 4, 5 ]
                        |> Maybe.andThen next
                        |> Maybe.andThen next
                        |> Maybe.andThen previous
                        |> Maybe.map current
                        |> Expect.equal (Just 2)
            , test "returns a Nothing if the selected is the first" <|
                \_ ->
                    singleton 1
                        |> previous
                        |> Expect.equal Nothing
            , test "does not change the items, only selected one" <|
                \_ ->
                    fromList [ 1, 2, 3, 4, 5 ]
                        |> Maybe.andThen next
                        |> Maybe.andThen next
                        |> Maybe.andThen previous
                        |> Maybe.andThen previous
                        |> Maybe.map toList
                        |> Expect.equal (Just [ 1, 2, 3, 4, 5 ])
            ]
        , describe "first"
            [ test "returns a Just ListZipper with the first item selected" <|
                \_ ->
                    fromList [ 1, 2, 3, 4, 5 ]
                        |> Maybe.andThen next
                        |> Maybe.andThen next
                        |> Maybe.map first
                        |> Maybe.map current
                        |> Expect.equal (Just 1)
            , test "does not change the items, only selected one" <|
                \_ ->
                    fromList [ 1, 2, 3, 4, 5 ]
                        |> Maybe.map first
                        |> Maybe.map toList
                        |> Expect.equal (Just [ 1, 2, 3, 4, 5 ])
            , test "does not change the items, only selected one, even after moving around" <|
                \_ ->
                    fromList [ 1, 2, 3, 4, 5 ]
                        |> Maybe.andThen next
                        |> Maybe.andThen next
                        |> Maybe.map first
                        |> Maybe.map toList
                        |> Expect.equal (Just [ 1, 2, 3, 4, 5 ])
            ]
        , describe "last"
            [ test "returns a Just ListZipper with the last item selected" <|
                \_ ->
                    fromList [ 1, 2, 3, 4, 5 ]
                        |> Maybe.andThen next
                        |> Maybe.andThen next
                        |> Maybe.map last
                        |> Maybe.map current
                        |> Expect.equal (Just 5)
            , test "does not change the items, only selected one" <|
                \_ ->
                    fromList [ 1, 2, 3, 4, 5 ]
                        |> Maybe.map last
                        |> Maybe.map toList
                        |> Expect.equal (Just [ 1, 2, 3, 4, 5 ])
            , test "does not change the items, only selected one, even after moving around" <|
                \_ ->
                    fromList [ 1, 2, 3, 4, 5 ]
                        |> Maybe.andThen next
                        |> Maybe.andThen next
                        |> Maybe.map last
                        |> Maybe.map toList
                        |> Expect.equal (Just [ 1, 2, 3, 4, 5 ])
            ]
        , describe "find"
            [ test "returns a Just ListZipper with the matched item selected" <|
                \_ ->
                    fromList [ 1, 2, 3, 4, 5 ]
                        |> Maybe.andThen (find ((==) 3))
                        |> Maybe.map current
                        |> Expect.equal (Just 3)
            , test "returns a Nothing if there's no matching item" <|
                \_ ->
                    singleton 1
                        |> find ((==) 3)
                        |> Expect.equal Nothing
            , test "does not change the items, only selected one" <|
                \_ ->
                    fromList [ 1, 2, 3, 4, 5 ]
                        |> Maybe.andThen (find ((==) 3))
                        |> Maybe.map toList
                        |> Expect.equal (Just [ 1, 2, 3, 4, 5 ])
            ]
        ]

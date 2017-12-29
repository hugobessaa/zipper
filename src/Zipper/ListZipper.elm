module Zipper.ListZipper exposing (..)

import List


type ListZipper a
    = ListZipper (List a) a (List a)


singleton a =
    ListZipper [] a []



-- ACCESSORS


before (ListZipper bfr _ _) =
    bfr


current (ListZipper _ curr _) =
    curr


after (ListZipper _ _ aft) =
    aft



-- LIST


fromList l =
    case ( List.head l, List.tail l ) of
        ( Just curr, Just after ) ->
            Just <| ListZipper [] curr after

        ( Just curr, Nothing ) ->
            Just <| ListZipper [] curr []

        _ ->
            Nothing


toList (ListZipper bfr curr aft) =
    bfr ++ [ curr ] ++ aft



-- MAPPING
-- map


map mapFn (ListZipper bfr curr aft) =
    ListZipper
        (List.map mapFn bfr)
        (mapFn curr)
        (List.map mapFn aft)



-- mapBefore
-- mapCurrent
-- mapAfter
-- mapIndexed
-- MOVING AROUND
-- first


first (ListZipper bfr curr aft) =
    if bfr == [] then
        ListZipper bfr curr aft
    else
        ListZipper
            []
            (List.head bfr |> Maybe.withDefault curr)
            (List.drop 1 bfr ++ [ curr ] ++ aft)



-- previous


previous (ListZipper bfr curr aft) =
    let
        ( b, c ) =
            case bfr of
                [] ->
                    ( Nothing, Nothing )

                b ->
                    ( Just <| List.take (List.length bfr - 1) bfr
                    , List.drop (List.length bfr - 1) bfr |> List.head
                    )
    in
    Maybe.map3
        ListZipper
        b
        c
        (Just (curr :: aft))



-- next


next (ListZipper bfr curr aft) =
    Maybe.map3
        ListZipper
        (Just (bfr ++ [ curr ]))
        (List.head aft)
        (List.tail aft)



-- last


last (ListZipper bfr curr aft) =
    if aft == [] then
        ListZipper bfr curr aft
    else
        ListZipper
            (bfr ++ [ curr ] ++ List.take (List.length aft - 1) aft)
            (List.drop (List.length aft - 1) aft
                |> List.head
                |> Maybe.withDefault curr
            )
            []



-- find


find findFn zipper =
    let
        recur =
            Maybe.andThen
                (\z ->
                    if findFn (current z) then
                        Just z
                    else
                        recur (next z)
                )
    in
    zipper
        |> first
        |> Just
        |> recur

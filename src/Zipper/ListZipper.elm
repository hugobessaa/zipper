module Zipper.ListZipper exposing (..)

{-| A zipper for `List`

The `ListZipper` type
@docs ListZipper

Constructing a `ListZipper`
@docs singleton, fromList

Accessors
@docs before, current, after, toList

Mapping
@docs map, mapBefore, mapCurrent, mapAfter

Moving around
@docs first, previous, next, last, find

-}

import List


{-| The `ListZipper` type.
-}
type ListZipper a
    = ListZipper (List a) a (List a)



-- CONSTRUCTORS


{-| Creates a `ListZipper` focussed on the first element of a singleton list.
-}
singleton : a -> ListZipper a
singleton a =
    ListZipper [] a []


{-| May construct a `ListZipper a` from a `List a`.
-}
fromList : List a -> Maybe (ListZipper a)
fromList list =
    case ( List.head list, List.tail list ) of
        ( Just curr, Just after ) ->
            Just <| ListZipper [] curr after

        ( Just curr, Nothing ) ->
            Just <| ListZipper [] curr []

        _ ->
            Nothing



-- ACCESSORS


{-| Returns all elements before the element `ListZipper` is focussed on.
-}
before : ListZipper a -> List a
before (ListZipper bfr _ _) =
    bfr


{-| Returns the element `ListZipper` is focussed on.
-}
current : ListZipper a -> a
current (ListZipper _ curr _) =
    curr


{-| Returns all elements after the element `ListZipper` is focussed on.
-}
after : ListZipper a -> List a
after (ListZipper _ _ aft) =
    aft


{-| Returns all elements inside the `ListZipper`.
-}
toList : ListZipper a -> List a
toList (ListZipper bfr curr aft) =
    bfr ++ [ curr ] ++ aft



-- MAPPING


{-| Maps a function over every element of the `ListZipper`.
-}
map : (a -> b) -> ListZipper a -> ListZipper b
map mapFn (ListZipper bfr curr aft) =
    ListZipper
        (List.map mapFn bfr)
        (mapFn curr)
        (List.map mapFn aft)


{-| Maps a function over every element before the element `ListZipper` is focussed on.
-}
mapBefore : (a -> a) -> ListZipper a -> ListZipper a
mapBefore mapFn (ListZipper bfr curr aft) =
    ListZipper
        (List.map mapFn bfr)
        curr
        aft


{-| Maps a function the element `ListZipper`is focussed on.
-}
mapCurrent : (a -> a) -> ListZipper a -> ListZipper a
mapCurrent mapFn (ListZipper bfr curr aft) =
    ListZipper
        bfr
        (mapFn curr)
        aft


{-| Maps a function over every element after the element `ListZipper` is focussed on.
-}
mapAfter : (a -> a) -> ListZipper a -> ListZipper a
mapAfter mapFn (ListZipper bfr curr aft) =
    ListZipper
        bfr
        curr
        (List.map mapFn aft)



-- mapIndexed
-- MOVING AROUND


{-| Focus on the first element of the `ListZipper`.
-}
first : ListZipper a -> ListZipper a
first (ListZipper bfr curr aft) =
    if bfr == [] then
        ListZipper bfr curr aft
    else
        ListZipper
            []
            (List.head bfr |> Maybe.withDefault curr)
            (List.drop 1 bfr ++ [ curr ] ++ aft)


{-| May focus on the previous element of the `ListZipper`. Returns `Nothing` if there is no previous element.
-}
previous : ListZipper a -> Maybe (ListZipper a)
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


{-| May focus on the next element of the `ListZipper`. Returns `Nothing` if there is no next element.
-}
next : ListZipper a -> Maybe (ListZipper a)
next (ListZipper bfr curr aft) =
    Maybe.map3
        ListZipper
        (Just (bfr ++ [ curr ]))
        (List.head aft)
        (List.tail aft)


{-| Focus on the last element of the `ListZipper`.
-}
last : ListZipper a -> ListZipper a
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


{-| Returns a `ListZipper` focussed on the first element for which the predicate returns `True`.
-}
find : (a -> Bool) -> ListZipper a -> Maybe (ListZipper a)
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

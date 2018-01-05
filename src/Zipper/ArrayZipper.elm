module Zipper.ArrayZipper exposing (..)

{-| A zipper for `Array`
-}

import Array exposing (Array)


type ArrayZipper a
    = ArrayZipper (Array a) a (Array a)



-- CONSTRUCTORS


{-| Creates a `ArrayZipper` focussed on the first element of a singleton array.
-}
singleton : a -> ArrayZipper a
singleton a =
    ArrayZipper Array.empty a Array.empty


{-| May construct a `ArrayZipper a` from a `Array a`.
-}
fromArray : Array a -> Maybe (ArrayZipper a)
fromArray array =
    let
        head =
            Array.get 0 array

        tail =
            Array.slice 1 (Array.length array) array
    in
    case head of
        Nothing ->
            Nothing

        Just head ->
            Just <| ArrayZipper Array.empty head tail



-- ACCESSORS


{-| Returns the element `ArrayZipper` is focussed on.
-}
current : ArrayZipper a -> a
current (ArrayZipper _ curr _) =
    curr


{-| Returns all elements before the element `ArrayZipper` is focussed on.
-}
before : ArrayZipper a -> Array a
before (ArrayZipper bfr _ _) =
    bfr


{-| Returns all elements after the element `ArrayZipper` is focussed on.
-}
after : ArrayZipper a -> Array a
after (ArrayZipper _ _ aft) =
    aft


{-| Returns all elements inside the `ArrayZipper`.
-}
toArray : ArrayZipper a -> Array a
toArray (ArrayZipper bfr curr aft) =
    bfr
        |> Array.push curr
        |> flip Array.append aft



-- MAPPING


{-| Maps a function over every element of the `ListZipper`.
-}
map : (a -> b) -> ArrayZipper a -> ArrayZipper b
map mapFn (ArrayZipper bfr curr aft) =
    ArrayZipper
        (Array.map mapFn bfr)
        (mapFn curr)
        (Array.map mapFn aft)


{-| Maps a function over every element before the element `ArrayZipper` is focussed on.
-}
mapBefore : (Array a -> Array a) -> ArrayZipper a -> ArrayZipper a
mapBefore mapFn (ArrayZipper bfr curr aft) =
    ArrayZipper
        (mapFn bfr)
        curr
        aft


{-| Maps a function the element `ArrayZipper`is focussed on.
-}
mapCurrent : (a -> a) -> ArrayZipper a -> ArrayZipper a
mapCurrent mapFn (ArrayZipper bfr curr aft) =
    ArrayZipper
        bfr
        (mapFn curr)
        aft


{-| Applies a function to all elements before the element `ArrayZipper` is focussed on.
-}
mapAfter : (Array a -> Array a) -> ArrayZipper a -> ArrayZipper a
mapAfter mapFn (ArrayZipper bfr curr aft) =
    ArrayZipper
        bfr
        curr
        (mapFn aft)



-- MOVING AROUND


{-| May focus on the next element of the `ArrayZipper`. Returns `Nothing` if there is no next element.
-}
next : ArrayZipper a -> Maybe (ArrayZipper a)
next (ArrayZipper bfr curr aft) =
    Maybe.map3
        ArrayZipper
        (Just (Array.push curr bfr))
        (Array.get 0 aft)
        (Just (Array.slice 1 (Array.length aft) aft))


{-| Focus on the last element of the `ListZipper`.
-}
last : ArrayZipper a -> ArrayZipper a
last (ArrayZipper bfr curr aft) =
    case Array.get (Array.length aft - 1) aft of
        Nothing ->
            ArrayZipper bfr curr aft

        Just last ->
            ArrayZipper
                (bfr
                    |> Array.push curr
                    |> flip Array.append (Array.slice 0 -1 aft)
                )
                last
                Array.empty

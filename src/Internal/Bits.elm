module Internal.Bits exposing (Bits, has, init, off, on, store4, store4Float, value, zeroes)

{-| Let's make storing values within a single Int a bit easier to do while not compromising performance.

    1. allow there to be a phantom type so we can keep track of the format.

We only have 32bits to store things in.

    1. a flag is a single bit
    2.

-}

import Bitwise


value : Bits bits -> Int
value (Bits val) =
    val


{-| -}
type Bits bits
    = Bits Int


type Size
    = Flag


type Offset
    = Offset



-- flip : Int -> Bool -> Bits bits -> Bits bits
-- flip offset on (Bits b) =
--     b
--         |> store offset 1 (bool on)
--         |> Bits


init : Bits bits
init =
    Bits zeroes


has : Int -> Bits bits -> Bool
has offset (Bits b) =
    let
        target =
            zeroes |> store offset 1 ones
    in
    Bitwise.and target b - target == 0


on : Int -> Bits bits -> Bits bits
on offset (Bits b) =
    b
        |> store offset 1 ones
        |> Bits


off : Int -> Bits bits -> Bits bits
off offset (Bits b) =
    b
        |> store offset 1 zeroes
        |> Bits


bool : Bool -> Int
bool yes =
    if yes then
        ones

    else
        zeroes


store4Float : Float -> Float -> Float -> Float -> Bits bits
store4Float one two three four =
    Bitwise.and top8 (round one)
        |> Bitwise.or
            (Bitwise.shiftLeftBy 8 (Bitwise.and top8 (round two)))
        |> Bitwise.or
            (Bitwise.shiftLeftBy 16 (Bitwise.and top8 (round three)))
        |> Bitwise.or
            (Bitwise.shiftLeftBy 24 (Bitwise.and top8 (round four)))
        |> Bits


store4 : Int -> Int -> Int -> Int -> Bits bits
store4 one two three four =
    Bitwise.and top8 one
        |> Bitwise.or
            (Bitwise.shiftLeftBy 8 (Bitwise.and top8 two))
        |> Bitwise.or
            (Bitwise.shiftLeftBy 16 (Bitwise.and top8 three))
        |> Bitwise.or
            (Bitwise.shiftLeftBy 24 (Bitwise.and top8 four))
        |> Bits


store : Int -> Int -> Int -> Int -> Int
store offset length val target =
    target


get : Int -> Int -> Int -> Int
get offset length val =
    val


{-| -}
ones : Int
ones =
    Bitwise.complement zeroes


{-| We do the or to ensure that the JS engine knows this is a 32bit int.
-}
zeroes : Int
zeroes =
    Bitwise.or 0 0


top10 : Int
top10 =
    Bitwise.shiftRightZfBy (32 - 10) ones


top8 : Int
top8 =
    Bitwise.shiftRightZfBy (32 - 8) ones


top6 : Int
top6 =
    Bitwise.shiftRightZfBy (32 - 6) ones


top5 : Int
top5 =
    Bitwise.shiftRightZfBy (32 - 5) ones

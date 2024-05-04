module InternalAnim.Bits exposing (Bits, store4Float, value)

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



-- flip : Int -> Bool -> Bits bits -> Bits bits
-- flip offset on (Bits b) =
--     b
--         |> store offset 1 (bool on)
--         |> Bits


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


{-| -}
ones : Int
ones =
    Bitwise.complement zeroes


{-| We do the or to ensure that the JS engine knows this is a 32bit int.
-}
zeroes : Int
zeroes =
    Bitwise.or 0 0


top8 : Int
top8 =
    Bitwise.shiftRightZfBy (32 - 8) ones

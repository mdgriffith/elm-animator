module Internal.Personality exposing
    ( Personality
    , default
    , linear
    , withArriveSmoothly
    , withDepartSmoothly
    , withImpulse
    , withWobble
    )

{-| -}

import Bitwise
import Internal.Bits as Bits


{-| Personality is basically a bitfield that tracks some numbers.

First 2 bits:

    - Has arrival been set
    - Has departure been set

These are used to allow for some default overrides.

We then have

Either

  - wobbliness : 0-127 (14bit)
      - OR -

  - arriveSmoothly 0-127 (7bit)

  - departSmoothly 0-127 (7bit)

And then finally

  - impulse 0-2048 (11bit) (initial velocity)

-}
type Personality
    = Personality Int


linear : Personality
linear =
    Personality (Bitwise.or 0 0)


default : Personality
default =
    Personality 0
        |> withArriveSmoothly 0.8
        |> withDepartSmoothly 0.4


withWobble : Float -> Personality -> Personality
withWobble wob (Personality p) =
    -- flip first two bits to 1
    -- set next 14 bits to
    Personality
        (p
            |> Bits.store 2 14 (round (wob * 16384))
        )


withImpulse : Float -> Personality -> Personality
withImpulse imp (Personality p) =
    Personality
        (p
            |> Bits.store 16 16 (round (imp * 65536))
        )


withArriveSmoothly : Float -> Personality -> Personality
withArriveSmoothly smooth (Personality p) =
    Personality
        (p
            |> Bits.store 0 1 1
            |> Bits.store 2 7 (round (smooth * 128))
        )


withDepartSmoothly : Float -> Personality -> Personality
withDepartSmoothly smooth (Personality p) =
    Personality
        (p
            |> Bits.store 1 1 1
            |> Bits.store 2 7 (round (smooth * 128))
        )


getArriveSmoothness : Personality -> Float
getArriveSmoothness (Personality p) =
    0


getDepartSmoothness : Personality -> Float
getDepartSmoothness (Personality p) =
    0


hasWobble : Personality -> Bool
hasWobble (Personality p) =
    False


getWobble : Personality -> Float
getWobble (Personality p) =
    0


getImpulse : Personality -> Float
getImpulse (Personality p) =
    0


{-| We have defaults because we want to set a default on the constructor

    `linear` or `move` have separate defaults

We don't need to worry about the `linear` default, because that's 0-ed out and we can safetly assume that any value set is an override for that.

For

-}
withDefault : Personality -> Personality -> Personality
withDefault (Personality one) (Personality two) =
    Personality one

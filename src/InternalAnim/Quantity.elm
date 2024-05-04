module InternalAnim.Quantity exposing
    ( Quantity(..)
    , divideBy
    , equalWithin
    , greaterThan
    , greaterThanOrEqualTo
    , lessThanOrEqualTo
    , max
    , minus
    , multiplyBy
    , plus
    , zero
    )


type Quantity number units
    = Quantity number


zero : Quantity number units
zero =
    Quantity 0


{-| -}
plus : Quantity number units -> Quantity number units -> Quantity number units
plus (Quantity y) (Quantity x) =
    Quantity (x + y)


minus : Quantity number units -> Quantity number units -> Quantity number units
minus (Quantity y) (Quantity x) =
    Quantity (x - y)


{-| -}
greaterThan : Quantity number units -> Quantity number units -> Bool
greaterThan (Quantity y) (Quantity x) =
    x > y


{-| -}
lessThanOrEqualTo : Quantity number units -> Quantity number units -> Bool
lessThanOrEqualTo (Quantity y) (Quantity x) =
    x <= y


{-| -}
greaterThanOrEqualTo : Quantity number units -> Quantity number units -> Bool
greaterThanOrEqualTo (Quantity y) (Quantity x) =
    x >= y


max : Quantity number units -> Quantity number units -> Quantity number units
max (Quantity x) (Quantity y) =
    Quantity (Basics.max x y)


multiplyBy : number -> Quantity number units -> Quantity number units
multiplyBy scale (Quantity value) =
    Quantity (scale * value)


equalWithin : Quantity number units -> Quantity number units -> Quantity number units -> Bool
equalWithin (Quantity tolerance) (Quantity x) (Quantity y) =
    Basics.abs (x - y) <= tolerance


divideBy : Float -> Quantity Float units -> Quantity Float units
divideBy divisor (Quantity value) =
    Quantity (value / divisor)

module Internal.Quantity exposing
    ( Quantity(..)
    , divideBy
    , equalWithin
    , greaterThan
    , greaterThanOrEqualTo
    , greaterThanOrEqualToZero
    , greaterThanZero
    , lessThan
    , lessThanOrEqualTo
    , lessThanOrEqualToZero
    , lessThanZero
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
lessThan : Quantity number units -> Quantity number units -> Bool
lessThan (Quantity y) (Quantity x) =
    x < y


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


{-| -}
lessThanZero : Quantity number units -> Bool
lessThanZero (Quantity x) =
    x < 0


{-| -}
greaterThanZero : Quantity number units -> Bool
greaterThanZero (Quantity x) =
    x > 0


{-| -}
lessThanOrEqualToZero : Quantity number units -> Bool
lessThanOrEqualToZero (Quantity x) =
    x <= 0


{-| -}
greaterThanOrEqualToZero : Quantity number units -> Bool
greaterThanOrEqualToZero (Quantity x) =
    x >= 0


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

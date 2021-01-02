module Internal.Spring exposing
    ( SpringParams
    , analytical
    , criticalDamping
    , peaks
    , segments
    , select
    , settlesAt
    , step
    , stepOver
    , wobble2Damping
    )

import Duration
import Internal.Bezier as Bezier
import Internal.Time as Time


{-| Calculate the position and velocity analytically instead of iteratively.

Much faster!

    https :// ellie - app.com / bNgBDt7GspVa1

-}
analytical :
    SpringParams
    -> Time.Duration
    -> Float
    ->
        { velocity : Float
        , position : Float
        }
    ->
        { velocity : Float
        , position : Float
        }
analytical spring duration target initial =
    let
        c1 =
            -- offset
            target - initial.position

        t =
            Duration.inMilliseconds duration * magicNumber / 1000

        magicNumber =
            Basics.sqrt (spring.stiffness / spring.mass)

        dampingRatio =
            spring.damping / (2 * Basics.sqrt (spring.mass * spring.stiffness))

        inner =
            Basics.sqrt
                (1 - (dampingRatio ^ 2))

        cosinElement =
            c1
                * Basics.cos (inner * t)

        c2 =
            dampingRatio * (c1 / inner)

        sinElement =
            c2 * Basics.sin (inner * t)

        newPosition =
            (Basics.e ^ ((-1 * dampingRatio) * t))
                * (cosinElement + sinElement)

        -- Calculate velocity
        attentuationFactor =
            Basics.e ^ ((-1 * dampingRatio) * t)

        firstFactor =
            ((dampingRatio * c1) - (c2 * inner))
                * (Basics.cos <| inner * t)

        secondFactor =
            ((dampingRatio * c2) + (c1 * inner))
                * (Basics.sin <| inner * t)

        newVelocity =
            attentuationFactor
                * (firstFactor + secondFactor)
    in
    { position = newPosition
    , velocity = newVelocity * magicNumber
    }


{-| -}
segments :
    SpringParams
    -> { position : Float, velocity : Float }
    -> Float
    -> List Bezier.Spline
segments spring initialState targetPos =
    let
        pks =
            peaks spring
                0
                targetPos
                initialState
    in
    List.map2
        (\one two ->
            let
                posOne =
                    analytical spring
                        (Duration.milliseconds one)
                        targetPos
                        initialState

                posTwo =
                    analytical spring
                        (Duration.milliseconds two)
                        targetPos
                        initialState

                -- factor = 0.05
                -- spread = 0.1
                factor =
                    0

                spread =
                    0

                offsetOne =
                    (two - one)
                        -- * 0.257
                        * ((0.33 - factor) - spread)

                ctrlOne =
                    -- + 90
                    Bezier.Point (one + offsetOne) (targetPos - posOne.position)

                offsetTwo =
                    (two - one)
                        -- * 0.55182845698119
                        * ((0.55 + factor) - spread)

                ctrlTwo =
                    -- - 197
                    Bezier.Point (two - offsetTwo) (targetPos - posTwo.position)

                -- 140
                -- 355
                -- 140 / 355 -> 0.39
                -- (355 - 140) / 355 -> 0.605
            in
            Bezier.Spline
                (Bezier.Point one (targetPos - posOne.position))
                ctrlOne
                ctrlTwo
                (Bezier.Point two (targetPos - posTwo.position))
        )
        pks
        (List.drop 1 pks)


zeroPoints :
    SpringParams
    -> Float
    -> Float
    ->
        { velocity : Float
        , position : Float
        }
    -> List Float
zeroPoints spring ms target initial =
    let
        inner =
            Basics.sqrt
                (1 - (dampingRatio ^ 2))

        c1 =
            -- offset
            target - initial.position

        c2 =
            dampingRatio * (c1 / inner)

        dampingRatio =
            spring.damping / (2 * Basics.sqrt (spring.mass * spring.stiffness))

        t k =
            -1 * (1 / Basics.sqrt (1 - (dampingRatio ^ 2))) * (Basics.atan (c1 / c2) - k * pi)

        magicNumber =
            Basics.sqrt (spring.stiffness / spring.mass)
    in
    [ (t 0 * 1000)
        / magicNumber
    , (t 1 * 1000)
        / magicNumber
    , (t 2 * 1000)
        / magicNumber
    , (t 3 * 1000)
        / magicNumber
    , (t 4 * 1000)
        / magicNumber
    ]


peaks :
    SpringParams
    -> Float
    -> Float
    ->
        { velocity : Float
        , position : Float
        }
    -> List Float
peaks spring ms target initial =
    let
        inner =
            Basics.sqrt
                (1 - (dampingRatio ^ 2))

        c1 =
            -- offset
            target - initial.position

        c2 =
            dampingRatio * (c1 / inner)

        dampingRatio =
            spring.damping / (2 * Basics.sqrt (spring.mass * spring.stiffness))

        magicNumber =
            Basics.sqrt (spring.stiffness / spring.mass)

        t k =
            (-1 / inner)
                * (Basics.atan
                    (top / bottom)
                    - k
                    * Basics.pi
                  )

        top =
            (dampingRatio * c1) - (c2 * inner)

        bottom =
            (dampingRatio * c2) + (c1 * inner)
    in
    [ (t 0 * 1000)
        / magicNumber
    , (t 1 * 1000)
        / magicNumber
    , (t 2 * 1000)
        / magicNumber
    , (t 3 * 1000)
        / magicNumber
    , (t 4 * 1000)
        / magicNumber
    ]


{-| Whew, math. Exciting isn't it?

Specifically, springs.

Let's first get a bunch of equations out here so we have some facts about how things relate.

But first, definitions

    - k    -> the spring constant, also known as stiffness
    - c    -> damping factor
    - c*   -> critical damping factor
    - m    -> mass (usually 1 for us)
    - z    -> damping ratio
    - ts   -> time to settle
    - w(n) -> the natural frequency of the system

A spring is usally described as the sum of 2 forces, the spring force and the damping force.

The spring force is what causes somethig to wiggle around.
The damping is what causes the spring to finally come to rest.

    F =
        (-k * x) + (-c * v)

    w(n) =
        sqrt (k / m)

    (m * ddx) + (c * dx) + (k * x) = 0

we can calculate the settling time.

    ts = 4 / (z * w(n))

    z = c / c*

    c* = 2 * sqrt (k * m)

    z = c / 2 * sqrt (m * k)

Expanding our ts equation:

    ts =
        4 / ((c / 2 * sqrt (m * k)) * sqrt (k / m))


# Our Usecase!

So, we have a lot of equations here. What is our ideal scenario?

When interpolating a timeline and heading towards a resting state, we want to use a spring.


## Cauchy problems:

Differential equations with bounding conditions!
<https://en.wikipedia.org/wiki/Cauchy_boundary_condition>

-}
step :
    Float
    -> SpringParams
    -> Float
    ->
        { velocity : Float
        , position : Float
        }
    ->
        { velocity : Float
        , position : Float
        }
step target { stiffness, damping, mass } dtms motion =
    let
        dt =
            dtms / 1000

        fspring =
            stiffness * (target - motion.position)

        fdamper =
            (-1 * damping) * motion.velocity

        a =
            (fspring + fdamper) / mass

        newVelocity =
            motion.velocity + (a * dt)

        newPos =
            motion.position + (newVelocity * dt)
    in
    { position = newPos
    , velocity = newVelocity
    }


stepOver :
    Time.Duration
    -> SpringParams
    -> Float
    ->
        { velocity : Float
        , position : Float
        }
    ->
        { velocity : Float
        , position : Float
        }
stepOver duration params target state =
    let
        durMS =
            Duration.inMilliseconds duration

        frames =
            durMS / 16

        remainder =
            16 * (frames - toFloat (floor frames))

        steps =
            if remainder > 0 then
                remainder :: List.repeat (floor durMS // 16) 16

            else
                List.repeat (floor durMS // 16) 16
    in
    List.foldl (step target params) state steps


type alias SpringParams =
    { stiffness : Float
    , damping : Float
    , mass : Float
    }


toleranceForSpringSettleTimeCalculation : Float
toleranceForSpringSettleTimeCalculation =
    -- this is about 4 for 2%
    -- however, we want a smaller tolerance
    -- this is 0.5% tolerance
    -1 * logBase e 0.005


{-|

    We can detect when a spring will settle if it is underdamped (meaning it oscillates before resting)
    <https://en.wikipedia.org/wiki/Settling_time>

    However for critcally and overdamped systems it gets a lot more complicated.
    Fortunately, I'm not sure that that's an issue as I don't think we want to model overdamped spring systems for animation.
    https://electronics.stackexchange.com/questions/296567/over-and-critically-damped-systems-settling-time

-}
settlesAt : SpringParams -> Float
settlesAt { stiffness, damping, mass } =
    let
        k =
            stiffness

        c =
            damping

        cCritical =
            criticalDamping k m

        m =
            mass

        springAspect =
            sqrt (k / m)
    in
    if round c == round cCritical then
        --critically damped
        -- less tolerant
        -- 1000 * (5.8335 / springAspect)
        -- more tolerant
        1000 * (8.5 / springAspect)

    else if (c - cCritical) > 0 then
        -- overdamped
        -- *NOTE* this branch is definitely not correct.
        -- this equation only applies to underdamped springs
        -- as far as I understand, this calculation for overdamped springs gets much more complicated
        -- However, I'm not sure how useful overdamped springs are in animation
        -- so we can prevent this branch by constraining the API.
        let
            dampingAspect =
                -- c / sqrt (m * k)
                c / cCritical
        in
        1000 * (toleranceForSpringSettleTimeCalculation / (dampingAspect * springAspect))

    else
        -- underdamped
        -- meaning it's wobbly
        let
            dampingAspect =
                -- c / sqrt (m * k)
                c / cCritical
        in
        1000
            * (toleranceForSpringSettleTimeCalculation
                / (dampingAspect * springAspect)
              )


{-| We need to select a spring based on wobbliness and desired settling time.

    wobbliness + settling -> { stiffness, mass (though 1 is nice)}

    stiffness: 120 - 210

        as stiffness increases, settling time shrinks
        between this number, the settling time is roughly linearly proportional to stiffness.
        We're going to keep this at 150 because changing it doesn't change the personality of the curve much.


    mass: ~1
        linearly related to settle time

    damping:
        calculabe from stiffness + mass + wobbliness


    wobbliness + stiffness + mass -> damping


    overdampening happens when the duration is short and the wobble is low.

-}
select : Float -> Time.Duration -> SpringParams
select wobbliness duration =
    let
        -- instead of worrying about varying stiffness
        -- we're just choosing a constant
        k =
            150

        damping =
            wobble2Damping wobbliness k 1 duration

        initiallySettlesAt =
            settlesAt
                { stiffness = k
                , damping = damping
                , mass = 1
                }

        newCritical =
            criticalDamping k (durMS / initiallySettlesAt)

        durMS =
            Duration.inMilliseconds duration
    in
    { stiffness = k
    , damping = damping

    -- we use the mass to scale the settling time to the desired duration
    , mass = durMS / initiallySettlesAt
    }


criticalDamping : Float -> Float -> Float
criticalDamping k m =
    2 * sqrt (k * m)



{- REDEFINING SPRING PARAMS

   Instead of stiffness and damping, we want to describe the motion that we'll have.

   1. pace ->
       fast/slow
   2. wobble ->
       wobbly
       noWobble

-}


{-| Wobble is essentally a damping ratio, but clamped to a certain range of values that are nice.

We take in the expected settlign time of the spring because at low settling times,
the range of the nice values changes

-}
wobble2Damping : Float -> Float -> Float -> Time.Duration -> Float
wobble2Damping wobble k m duration =
    wobble2Ratio wobble duration * criticalDamping k m


{-| wobble:

    0 -> no wobble
    0.5 -> some wobble
    1 -> all the wobble

ratio:

    0.8 -> basically no wobble
    0.43 -> max wobble (arbitrarily chosen)


    overdampening happens when the duration is short and the wobble is low.

    if duration is below 350ms, then a high ratio can lead to overdamping, which we don't want.

    So, we linearly scale the top bound if the duration is below 350ms.

-}
wobble2Ratio : Float -> Time.Duration -> Float
wobble2Ratio wobble duration =
    let
        bounded =
            wobble
                |> max 0
                |> min 1

        ms =
            Duration.inMilliseconds duration

        scalingBelowDur =
            ms / 350

        top =
            max 0.43 (0.8 * min 1 scalingBelowDur)
    in
    (1 - bounded)
        |> mapToRange 0.43 top


mapToRange minimum maximum x =
    let
        total =
            maximum - minimum
    in
    minimum + (x * total)



{-


    stiffness: 120 - 210
    damping : 12 - 26

      // borrowed from react motion for the moment
     export default {
       noWobble: [170, 26], // the default
      - cCritical: 26.06
      - ratio: ~1
       gentle: [120, 14],
      - cCritical: 21.9
      - ratio: 0.639
       wobbly: [180, 12],
      - cCritical: 26.83
      - ratio: 0.44
       stiff: [210, 20],
      - cCritical: 28.98
      - ratio: 0.69

     };



   cCritical = 2 * sqrt (k * m)








     f(0) = 0; f'(0) = 0; f''(t) = -170(f(t) - 1) - 26f'(t)

         -> e^(-13 t) (e^(13 t) - cos(t) - 13 sin(t))

     f(0) = 0; f'(0) = 0; f''(t) = -120(f(t) - 1) - 14f'(t)

         -> 1 - e^(-7 t) cos(sqrt(71) t) - (7 e^(-7 t) sin(sqrt(71) t))/sqrt(71)

     f(0) = 0; f'(0) = 0; f''(t) = -180(f(t) - 1) - 12f'(t)

     f(0) = 0; f'(0) = 0; f''(t) = -210(f(t) - 1) - 20f'(t)

         -> 1 - e^(-10 t) cos(sqrt(110) t) - sqrt(10/11) e^(-10 t) sin(sqrt(110) t)

-}
{- Deriving equations for the wobbly setting, varying initial conditions



    f(0) = 0; f'(0) = 0; f''(t) = -180(f(t) - 1) - 12f'(t)



   -- 0,0
           -1/2   e^(-6 t) (-2 e^(6 t)
            + 2 cos(12 t)
            + sin(12 t))


    f(0) = 100; f'(0) = 0; f''(t) = -180(f(t) - 1) - 12f'(t)
   -- 100, 0

           1
               + 99 e^(-6 t) cos(12 t)
               + 99/2 e^(-6 t) sin(12 t)

    f(0) = 100; f'(0) = 100; f''(t) = -180(f(t) - 1) - 12f'(t)
   -- 100, 100

           1
               + 99 e^(-6 t) cos(12 t)
               + 347/6 e^(-6 t) sin(12 t)


    f(0) = 0; f'(0) = 100; f''(t) = -180(f(t) - 1) - 12f'(t)
   -- 0, 100

           1 - e^(-6 t) cos(12 t)
               + 47/6 e^(-6 t) sin(12 t)


-}

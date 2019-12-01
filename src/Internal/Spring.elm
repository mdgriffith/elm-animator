module Internal.Spring exposing
    ( criticalDamping
    , settlesAt
    , step
    , wobble2Damping
    )

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


tolerance =
    0.1


vTolerance =
    0.01


step :
    Float
    ->
        { stiffness : Float
        , damping : Float
        }
    ->
        { target : Float
        , velocity : Float
        , position : Float
        }
    ->
        { target : Float
        , velocity : Float
        , position : Float
        }
step dtms { stiffness, damping } motion =
    let
        dt =
            dtms / 1000

        fspring =
            stiffness * (motion.target - motion.position)

        fdamper =
            (-1 * damping) * motion.velocity

        a =
            fspring + fdamper

        newVelocity =
            motion.velocity + (a * dt)

        newPos =
            motion.position + (newVelocity * dt)

        dx =
            abs (motion.target - newPos)
    in
    if dx < tolerance && abs newVelocity < vTolerance then
        { motion
            | position = motion.target
            , velocity = 0.0
        }

    else
        { motion
            | position = newPos
            , velocity = newVelocity
        }


{-|

    We can detect when a spring will settle if it is underdamped (meaning it oscillates before resting)
    <https://en.wikipedia.org/wiki/Settling_time>

    However for critcally and overdamped systems it gets a lot more complicated.
    Fortunately, I'm not sure that that's an issue as I don't think we want to model overdamped spring systems for animation.
    https://electronics.stackexchange.com/questions/296567/over-and-critically-damped-systems-settling-time

-}
settlesAt : { stiffness : Float, damping : Float } -> Float
settlesAt { stiffness, damping } =
    let
        k =
            stiffness

        c =
            damping

        cCritical =
            criticalDamping k m

        m =
            1

        springAspect =
            sqrt (k / m)
    in
    if round c == round cCritical then
        --critically damped
        -- less tolerant
        -- 1000 * (5.8335 / springAspect)
        -- more tolerant
        1000 * (8.5 / springAspect)

    else if c > cCritical then
        -- overdamped
        -- *NOTE* this branch is definitely not correct.
        -- this equation only applies to underdamped springs
        -- However, I'm not sure how useful overdamped springs are in animation
        -- so we can prevent this branch by constraining the API.
        let
            dampingAspect =
                -- c / sqrt (m * k)
                c / cCritical

            toleranceForEquation =
                -- this is about 4 for 2%
                -1 * logBase e 0.005
        in
        1000 * (toleranceForEquation / (dampingAspect * springAspect))

    else
        -- underdamped
        let
            dampingAspect =
                -- c / sqrt (m * k)
                c / cCritical

            toleranceForEquation =
                -- this is about 4 for 2%
                -- however, we want a smaller tolerance
                -- this is 0.5% tolerance
                -1 * logBase e 0.005
        in
        1000 * (toleranceForEquation / (dampingAspect * springAspect))


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


wobble2Damping wobble k m =
    wobble2Ratio wobble * criticalDamping k m


{-| wobble:
0 -> no wobble
0.5 -> some wobble
1 -> all the wobble

ratio:
1 -> nowobble

    0.4 -> max wobble (arbitrarily chosen)

-}
wobble2Ratio wobble =
    let
        bounded =
            wobble
                |> max 0
                |> min 1
    in
    1
        - bounded
        |> mapToRange 0.3 1


mapToRange minimum maximum x =
    let
        total =
            maximum - minimum
    in
    minimum + (x * total)



{-
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

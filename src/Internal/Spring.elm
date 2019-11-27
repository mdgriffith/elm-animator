module Internal.Spring exposing (..)

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

    z = c / sqrt (m * k)


# Our Usecase!

So, we have a lot of equations here. What is our ideal scenario?

When interpolating a timeline and heading towards a resting state, we want to use a spring.

-}


spring dtms { stiffness, damping } motion =
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



{-
    // borrowed from react motion for the moment
   export default {
     noWobble: [170, 26], // the default
     gentle: [120, 14],
     wobbly: [180, 12],
     stiff: [210, 20],
   };



   f(0) = 0; f'(0) = 0; f''(t) = -170(f(t) - 1) - 26f'(t)

       -> e^(-13 t) (e^(13 t) - cos(t) - 13 sin(t))

   f(0) = 0; f'(0) = 0; f''(t) = -120(f(t) - 1) - 14f'(t)

       -> 1 - e^(-7 t) cos(sqrt(71) t) - (7 e^(-7 t) sin(sqrt(71) t))/sqrt(71)

   f(0) = 0; f'(0) = 0; f''(t) = -180(f(t) - 1) - 12f'(t)

   f(0) = 0; f'(0) = 0; f''(t) = -210(f(t) - 1) - 20f'(t)

       -> 1 - e^(-10 t) cos(sqrt(110) t) - sqrt(10/11) e^(-10 t) sin(sqrt(110) t)

-}

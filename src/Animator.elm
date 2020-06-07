module Animator exposing
    ( Timeline, init
    , current, previous, upcoming, upcomingWith, arrived, arrivedAt, arrivedAtWith
    , Animator
    , animator, watching, watchingWith
    , toSubscription, update
    , updateTimeline
    , go
    , Duration, immediately, veryQuickly, quickly, slowly, verySlowly, millis, seconds
    , Step, wait, event
    , interrupt, queue
    , color
    , Movement, at, move, xy, xyz
    , linear
    , leaveLate, arriveEarly
    , leaveSmoothly, arriveSmoothly
    , withWobble
    , Oscillator, wave, wrap, zigzag, interpolate
    , loop, once, repeat
    , shift
    , step
    , Frames, frame, hold, walk, framesWith
    , Resting, FramesPerSecond, fps, cycle, cycleN
    )

{-|


# Getting started

`elm-animator` is about taking pieces of your model, turning them into **Timelines** of values, and animate between their states

@docs Timeline, init


# Reading the timeline

You might be wondering, 'How do we get our value "out" of a `Timeline`?'

Well, we can ask the `Timeline` all sorts of questions.

@docs current, previous, upcoming, upcomingWith, arrived, arrivedAt, arrivedAtWith


# Wiring up the animation

Once we have a `Timeline`, we need a way to update it.

That's the job of the `Animator`!

@docs Animator

@docs animator, watching, watchingWith

@docs toSubscription, update

@docs updateTimeline


# Transitioning to a new state

Now that we have a `Timeline` set up, we likely want to set a new **value**.

In order to do that we need to specify both —

  - the new state we want to be in
  - a `Duration` for how long this transition should take.

@docs go

@docs Duration, immediately, veryQuickly, quickly, slowly, verySlowly, millis, seconds


# Interruptions and Queueing

In some more **advanced** cases you might want to define a _series_ of states to animate through instead of just going to one directly.

    Animator.interrupt
        [ Animator.wait (Animator.millis 300)

        -- after waiting 300 milliseconds,
        -- start transitioning to a new state, Griffyndor
        -- Take 1 whole second to make the transition
        , Animator.event (Animator.seconds 1) Griffyndor

        -- Once we've arrived at Griffyndor,
        -- immediately start transitioning to Slytherin
        -- and take half a second to make the transition
        , Animator.event (Animator.seconds 0.5) Slytherin
        ]

@docs Step, wait, event

@docs interrupt, queue


# Animating

Finally, animating!

This part of the package is for animating color and number **values** directly.

_However!_ You're probably more interested in animating `CSS` or `Inline` styles.

Those things live in the `Animator.Css`and `Animator.Inline` modules.

Check them out on the side bar 👉

Though you should also check out the 👇 [Transition Personality](#transition-personality) section as well.

@docs color

@docs Movement, at, move, xy, xyz

@docs linear


# Transition personality

While there are some nice defaults baked in, sometimes you might want to adjust how an animation happens.

These adjustments talk about _arriving_ or _leaving_. That's referring to the part of the animation that is arriving to or departing from a certain state.

So, for this code example:

     case state of
        True ->
            Animator.at 0
                |> Animator.leaveLate 0.2

        False ->
           Animator.at 50

If we're at a state of `True` and go to any other state, we're going to leave `True` a little later than the normal time.

**Note** — These adjustments all take a `Float` between `0` and `1`. Behind the scenes they will be clamped at those values.

@docs leaveLate, arriveEarly

@docs leaveSmoothly, arriveSmoothly

@docs withWobble


# Resting at a state

We've mostly talked about **transitioning** from one state to another, like moving from `True` to `False`.

But what if we want an animation when we're just **resting** at a state?

An obvious example would be an icon that spins when we're `Loading`.

Well, in that case you can use an `Oscillator`.

    case state of
        Loaded ->
            Animator.at 0

        Loading ->
            -- animate from 0deg to 360deg and
            -- then wrap back around to 0deg
            -- we're using radians here, so 2 * pi == 360deg
            Animator.wrap 0 (2 * pi)
                -- loop every 700ms
                |> Animator.loop (Animator.millis 700)

@docs Oscillator, wave, wrap, zigzag, interpolate

Once we've created an oscillator, we need to specify how long it should take and how many times it should repeat.

@docs loop, once, repeat

@docs shift


# Sprites

Ok! What else could there be?

What about the wonderful world of Sprite animation?

Sprite animation is where we literally have a list of images and flip through them like a flip-book.

Like Mario! In fact we have a [Mario example](https://github.com/mdgriffith/elm-animator/blob/master/examples/Mario.elm)!

Here's an abreviated example of what the code looks like:

    Animator.step model.mario <|
        \(Mario action) ->
            case action of
                Walking ->
                    -- if we're in a `Walking` state,
                    -- then we're cycling through
                    -- the following frames at
                    -- 15 frames per second:
                    --  step1, step2, stand
                    Animator.framesWith
                        { transition =
                            sprite.tail.stand
                        , resting =
                            Animator.cycle
                                (Animator.fps 15)
                                [ sprite.tail.step1
                                , sprite.tail.step2
                                , sprite.tail.stand
                                ]
                        }

                Jumping ->
                    -- show a single frame
                    sprite.tail.jump

                Ducking ->
                    sprite.tail.duck

                Standing ->
                    sprite.tail.stand

@docs step

@docs Frames, frame, hold, walk, framesWith

@docs Resting, FramesPerSecond, fps, cycle, cycleN

-}

import Browser.Events
import Color exposing (Color)
import Duration
import Internal.Interpolate as Interpolate
import Internal.Time as Time
import Internal.Timeline as Timeline
import Quantity
import Time


{-| A timeline of `state` values.

Behind the scenes this is roughly a list of states and the times that they should occur!

-}
type alias Timeline state =
    Timeline.Timeline state


{-| Create a timeline with an initial `state`.

So, if you previously had a `Bool` in your model:

    type alias Model = { checked : Bool }

    -- created via
    { checked = False }

You could replace that with an `Animator.Timeline Bool`

    type alias Model = { checked : Animator.Timeline Bool }

    -- created via
    { checked = Animator.init False }

-}
init : state -> Timeline state
init first =
    Timeline.Timeline
        { initial = first
        , now = Time.absolute (Time.millisToPosix 0)
        , events =
            Timeline.Timetable []
        , queued = Nothing
        , interruption = []
        , running = True
        }


{-| -}
initWith : Time.Posix -> state -> Timeline state
initWith now first =
    Timeline.Timeline
        { initial = first
        , now = Time.absolute (Time.millisToPosix 0)
        , events =
            Timeline.Timetable []
        , queued = Nothing
        , interruption = []
        , running = True
        }
        |> Timeline.update now


{-| Get the current `state` of the timeline.

This value will switch to a new value when a transition begins.

If you had a timeline that went from A to B to C, here's what `current` would be at various points on the timeline.

```ascii
          A---------B---------C
               ^    ^    ^    ^
current:       B    B    C    C
```

**Note** — If you want to detect the moment when you arrive at a new state, try using [`arrivedAt`](#arrivedAt)

-}
current : Timeline state -> state
current =
    Timeline.current


{-| Subtley different than [`current`](#current), this will provide the new state as soon as the transition has _finished_.

```ascii
          A---------B---------C
               ^    ^    ^    ^
arrived:       A    B    B    C
```

-}
arrived : Timeline state -> state
arrived =
    Timeline.arrived


{-| Sometimes we want to know when we've arrived at a state so we can trigger some other work.

You can use `arrivedAt` in the `Tick` branch of your update to see if you will arrive at an event on this tick.

    Tick time ->
        if Animator.arrivedAt MyState time model.timeline then
            --...do something special

-}
arrivedAt : state -> Time.Posix -> Timeline state -> Bool
arrivedAt state =
    Timeline.arrivedAt ((==) state)


{-| Again, sometimes you'll want to supply your own equality function!
-}
arrivedAtWith : (state -> Bool) -> Time.Posix -> Timeline state -> Bool
arrivedAtWith =
    Timeline.arrivedAt


{-| Get the previous `state` on this timeline.

As you'll see in the [Loading example](https://github.com/mdgriffith/elm-animator/blob/master/examples/Loading.elm), it means we can use `previous` to refer to data that we've already "deleted" or set to `Nothing`.

How cool!

```ascii
          A---------B---------C
               ^    ^    ^
previous:      A    A    B
```

-}
previous : Timeline state -> state
previous =
    Timeline.previous


{-| Check to see if a `state` is upcoming on a timeline.

**Note** — This can be used to ensure a set of states can only be [`queued`](#queue) if they aren't already running.

**Note 2** — This only checks if an event is in the _future_, but does not check the value you're currently at. You might need to use [`arrived`](#arrived) as well if you also care about the current state.

-}
upcoming : state -> Timeline state -> Bool
upcoming state =
    Timeline.upcoming ((==) state)


{-| For complicated values it can be computationally expensive to use `==`.

`upcomingWith` allows you to specify your own equality function, so you can be smarter in checking how two value are equal.

-}
upcomingWith : (state -> Bool) -> Timeline state -> Bool
upcomingWith =
    Timeline.upcoming



-- future : Timeline state -> List ( TIme.Posix, state )


{-| Choosing a nice duration can depend on:

  - The size of the thing moving
  - The type of movement
  - The distance it's traveling.

So, start with a nice default and adjust it as you start to understand your specific needs.

**Note** — Here's [a very good overview on animation durations and speeds](https://uxdesign.cc/the-ultimate-guide-to-proper-use-of-animation-in-ux-10bd98614fa9).

-}
type alias Duration =
    Time.Duration


{-| -}
millis : Float -> Duration
millis =
    Duration.milliseconds


{-| -}
seconds : Float -> Duration
seconds =
    Duration.seconds


{-| -}
minutes : Float -> Duration
minutes =
    Duration.minutes


{-| -}
type Step state
    = Wait Duration
    | TransitionTo Duration state


{-| -}
event : Duration -> state -> Step state
event =
    TransitionTo


{-| -}
wait : Duration -> Step state
wait =
    Wait


{-| Wait until the current timeline is **finished** and then continue with these new steps.
-}
queue : List (Step state) -> Timeline state -> Timeline state
queue steps (Timeline.Timeline tl) =
    Timeline.Timeline
        { tl
            | running = True
            , queued =
                case tl.queued of
                    Nothing ->
                        case initializeSchedule (millis 0) steps of
                            Nothing ->
                                tl.queued

                            Just ( schedule, otherSteps ) ->
                                Just (List.foldl stepsToEvents schedule otherSteps)

                    Just queued ->
                        Just (List.foldl stepsToEvents queued steps)
        }


{-| 0ms
-}
immediately : Duration
immediately =
    millis 0


{-| _100ms_.
-}
veryQuickly : Duration
veryQuickly =
    millis 100


{-| _200ms_ - Likely a good place to start!
-}
quickly : Duration
quickly =
    millis 200


{-| Go to a new state!

You'll need to specify a `Duration` as well. Try starting with `Animator.quickly` and adjust up or down as necessary.

-}
go : Duration -> state -> Timeline state -> Timeline state
go duration ev timeline =
    interrupt [ event duration ev ] timeline


{-| _400ms_.
-}
slowly : Duration
slowly =
    millis 400


{-| _500ms_.
-}
verySlowly : Duration
verySlowly =
    millis 500


{-| Interrupt what's currently happening with a new list.
-}
interrupt : List (Step state) -> Timeline state -> Timeline state
interrupt steps (Timeline.Timeline tl) =
    Timeline.Timeline
        { tl
            | running = True
            , interruption =
                case initializeSchedule (millis 0) steps of
                    Nothing ->
                        tl.interruption

                    Just ( schedule, otherSteps ) ->
                        List.foldl stepsToEvents schedule otherSteps :: tl.interruption
        }


initializeSchedule : Time.Duration -> List (Step state) -> Maybe ( Schedule state, List (Step state) )
initializeSchedule waiting steps =
    case steps of
        [] ->
            Nothing

        (Wait additionalWait) :: moreSteps ->
            initializeSchedule
                (Quantity.plus waiting additionalWait)
                moreSteps

        (TransitionTo dur checkpoint) :: moreSteps ->
            Just ( Timeline.Schedule waiting (Timeline.Event dur checkpoint Nothing) [], moreSteps )


stepsToEvents : Step state -> Timeline.Schedule state -> Timeline.Schedule state
stepsToEvents currentStep (Timeline.Schedule delay startEvent events) =
    case events of
        [] ->
            case currentStep of
                Wait waiting ->
                    Timeline.Schedule
                        delay
                        (Timeline.extendEventDwell waiting startEvent)
                        events

                TransitionTo dur checkpoint ->
                    Timeline.Schedule
                        delay
                        startEvent
                        [ Timeline.Event dur checkpoint Nothing ]

        (Timeline.Event durationTo recentEvent maybeDwell) :: remaining ->
            case currentStep of
                Wait dur ->
                    Timeline.Schedule
                        delay
                        startEvent
                        (Timeline.Event durationTo recentEvent (Timeline.addToDwell dur maybeDwell) :: remaining)

                TransitionTo dur checkpoint ->
                    if checkpoint == recentEvent then
                        Timeline.Schedule
                            delay
                            startEvent
                            (Timeline.Event durationTo recentEvent (Timeline.addToDwell dur maybeDwell) :: remaining)

                    else
                        Timeline.Schedule
                            delay
                            startEvent
                            (Timeline.Event dur checkpoint Nothing :: events)


{-| -}
type alias Event state =
    Timeline.Event state


{-| -}
type alias Schedule state =
    Timeline.Schedule state



{- Interpolations -}


type alias Description state =
    Timeline.Description state


{-| -}
color : Timeline state -> (state -> Color) -> Color
color timeline lookup =
    Timeline.foldp
        lookup
        Interpolate.coloring
        timeline


{-| Interpolate linearly between destinations. This is a shortcut to help you out.

You can do this with `move` by doing

    Animator.move timeline <|
        \state ->
            if state then
                Animator.at 0
                    |> Animator.leaveSmoothly 0
                    |> Animator.arriveSmoothly 0

            else
                Animator.at 1
                    |> Animator.leaveSmoothly 0
                    |> Animator.arriveSmoothly 0

Which is equivalent to

    Animator.linear timeline <|
        \state ->
            if state then
                Animator.at 0

            else
                Animator.at 1

-}
linear : Timeline state -> (state -> Movement) -> Float
linear timeline lookup =
    .position <|
        Interpolate.details timeline
            (Interpolate.withLinearDefault << lookup)


{-| -}
move : Timeline state -> (state -> Movement) -> Float
move timeline lookup =
    .position <|
        Interpolate.details timeline
            (Interpolate.withStandardDefault << lookup)


{-| -}
xy : Timeline state -> (state -> { x : Movement, y : Movement }) -> { x : Float, y : Float }
xy timeline lookup =
    { x =
        Timeline.foldp
            (lookup >> .x >> Interpolate.withStandardDefault)
            Interpolate.moving
            timeline
            |> unwrapUnits
            |> .position
    , y =
        Timeline.foldp
            (lookup >> .y >> Interpolate.withStandardDefault)
            Interpolate.moving
            timeline
            |> unwrapUnits
            |> .position
    }


{-| -}
xyz : Timeline state -> (state -> { x : Movement, y : Movement, z : Movement }) -> { x : Float, y : Float, z : Float }
xyz timeline lookup =
    { x =
        Timeline.foldp
            (lookup >> .x >> Interpolate.withStandardDefault)
            Interpolate.moving
            timeline
            |> unwrapUnits
            |> .position
    , y =
        Timeline.foldp
            (lookup >> .y >> Interpolate.withStandardDefault)
            Interpolate.moving
            timeline
            |> unwrapUnits
            |> .position
    , z =
        Timeline.foldp
            (lookup >> .z >> Interpolate.withStandardDefault)
            Interpolate.moving
            timeline
            |> unwrapUnits
            |> .position
    }


unwrapUnits { position, velocity } =
    { position =
        case position of
            Quantity.Quantity val ->
                val
    , velocity =
        case velocity of
            Quantity.Quantity val ->
                val
    }


{-| -}
type alias Movement =
    Interpolate.DefaultableMovement


{-| -}
at : Float -> Movement
at =
    Interpolate.Position
        Interpolate.FullDefault


withDefault toDef currentDefault =
    case currentDefault of
        Interpolate.FullDefault ->
            Interpolate.PartialDefault (toDef Interpolate.emptyDefaults)

        Interpolate.PartialDefault thing ->
            Interpolate.PartialDefault (toDef thing)


applyOption toOption movement =
    case movement of
        Interpolate.Position personality pos ->
            Interpolate.Position
                (withDefault
                    toOption
                    personality
                )
                pos

        Interpolate.Oscillate personality dur fn ->
            Interpolate.Oscillate
                (withDefault
                    toOption
                    personality
                )
                dur
                fn



{- PERSONALITY -}


{-| This will make the transition use a spring instead of bezier curves!

  - `withWobble 0` - absolutely no wobble
  - `withWobble 1` - all the wobble

Use your wobble responsibly.

-}
withWobble : Float -> Movement -> Movement
withWobble p movement =
    applyOption (\def -> { def | wobbliness = Interpolate.Specified (clamp 0 1 p) }) movement



-- {-| -}
-- smooth : Float
-- smooth =
--     0.4
-- {-| -}
-- verySmooth : Float
-- verySmooth =
--     0.8


{-| Even though the transition officially starts at a certain time on the timeline, we can leave a little late.

  - `0` means we leave at the normal time.
  - `0.2` means we'll leave when the transition is at 20%.
  - `1` means we leave at the end of the transition and instantly flip to the new state at that time.

-}
leaveLate : Float -> Movement -> Movement
leaveLate p movement =
    applyOption (\def -> { def | departLate = Interpolate.Specified (clamp 0 1 p) }) movement


{-| We can also arrive early to this state.

  - `0` means we arrive at the normal time.
  - `0.2` means we'll arrive early by 20% of the total duration.
  - `1` means we arrive at the start of the transition. So basically we instantly transition over.

**Weird math note** — `arriveEarly` and `leaveLate` will collaborate to figure out how the transition happens. If `arriveEarly` and `leaveLate` sum up to more `1` for a transition, then their sum will be the new maximum. Likely you don't need to worry about this :D.

The intended use for `arriveEarly` and `leaveLate` is for staggering items in a list. In those cases, these values are pretty small `~0.1`.

-}
arriveEarly : Float -> Movement -> Movement
arriveEarly p movement =
    applyOption (\def -> { def | arriveEarly = Interpolate.Specified (clamp 0 1 p) }) movement


{-| Underneath the hood this library uses [Bézier curves](https://en.wikipedia.org/wiki/B%C3%A9zier_curve) to model motion.

Because of this you can adjust the "smoothness" of the curve that's ultimately used.

  - `leaveSmoothly 0` is essentially linear animation.
  - `leaveSmoothly 1` means the animation will start slowly and smoothly begin to accelerate.

Here's a general diagram of what's going on:

![](https://mdgriffith.github.io/elm-animator/images/default-personality.png)

**Note** — The values in the above diagram are the built in defaults for most movements in `elm-animator`. They come from [`Material Design`](https://material.io/design/motion/speed.html#easing).

**Note 2** — An [interactive version of the above diagram](https://ellie-app.com/8s2yjQzQmZda1) is also available.

-}
leaveSmoothly : Float -> Movement -> Movement
leaveSmoothly s movement =
    applyOption (\def -> { def | departSlowly = Interpolate.Specified (clamp 0 1 s) }) movement


{-| We can also smooth out our arrival.

  - `arriveSmoothly 0` means no smoothing, which means more of a linear animation.
  - `arriveSmoothly 1` means the animation will "ease out" or "arrive slowly".

-}
arriveSmoothly : Float -> Movement -> Movement
arriveSmoothly s movement =
    applyOption (\def -> { def | arriveSlowly = Interpolate.Specified (clamp 0 1 s) }) movement


{-| -}
type alias Oscillator =
    Timeline.Oscillator


{-| -}
type alias Pause =
    Timeline.Pause


within : Float -> Float -> Float -> Bool
within tolerance anchor val =
    let
        low =
            anchor - tolerance

        high =
            anchor + tolerance
    in
    val >= low && val <= high


{-| -}
once : Duration -> Oscillator -> Movement
once activeDuration osc =
    case osc of
        Timeline.Resting i ->
            at i

        Timeline.Oscillator pauses fn ->
            let
                ( preparedFn, totalDuration ) =
                    Timeline.prepareOscillator activeDuration pauses fn
            in
            Interpolate.Oscillate
                Interpolate.FullDefault
                (Timeline.Repeat 1 totalDuration)
                preparedFn


{-| -}
loop : Duration -> Oscillator -> Movement
loop activeDuration osc =
    case osc of
        Timeline.Resting i ->
            at i

        Timeline.Oscillator pauses fn ->
            let
                ( preparedFn, totalDuration ) =
                    Timeline.prepareOscillator activeDuration pauses fn
            in
            Interpolate.Oscillate
                Interpolate.FullDefault
                (Timeline.Loop totalDuration)
                preparedFn


{-| -}
repeat : Int -> Duration -> Oscillator -> Movement
repeat n activeDuration osc =
    case osc of
        Timeline.Resting i ->
            at i

        Timeline.Oscillator pauses fn ->
            let
                ( preparedFn, totalDuration ) =
                    Timeline.prepareOscillator activeDuration pauses fn
            in
            Interpolate.Oscillate
                Interpolate.FullDefault
                (Timeline.Repeat n totalDuration)
                preparedFn


{-| Shift an oscillator over by a certain amount.

It's expecting a number between 0 and 1.

-}
shift : Float -> Oscillator -> Oscillator
shift x osc =
    case osc of
        Timeline.Oscillator pauses fn ->
            Timeline.Oscillator
                pauses
                (\u -> fn (wrapToUnit (u + x)))

        Timeline.Resting _ ->
            osc


wrapToUnit : Float -> Float
wrapToUnit x =
    x - toFloat (floor x)


{-| Pause the the oscillator is at a certain point.

This pause time will be added to the time you specify using `loop`, so that you can adjust the pause without disturbing the original duration of the oscillator.

-}
pause : Duration -> Float -> Oscillator -> Oscillator
pause forDuration val osc =
    case osc of
        Timeline.Oscillator pauses fn ->
            Timeline.Oscillator
                (Timeline.Pause forDuration val :: pauses)
                fn

        Timeline.Resting _ ->
            osc


{-| Start at one number and move linearly to another, then immediately start again at the first.

This was originally intended for animating rotation where you'd want 360deg to "wrap" to 0deg.

-}
wrap : Float -> Float -> Oscillator
wrap start end =
    let
        total =
            end - start
    in
    Timeline.Oscillator []
        (\u ->
            if u == 1 then
                start

            else
                start + (total * u)
        )


{-| This is basically a sine wave! It will "wave" between the two numbers you give it.
-}
wave : Float -> Float -> Oscillator
wave start end =
    let
        top =
            max start end

        bottom =
            min start end

        total =
            top - bottom
    in
    Timeline.Oscillator []
        (\u ->
            let
                normalized =
                    (cos (turns (0.5 + u)) + 1) / 2
            in
            start + total * normalized
        )


{-| Start at one number, move linearly to another, and then linearly back.
-}
zigzag : Float -> Float -> Oscillator
zigzag start end =
    let
        total =
            end - start
    in
    Timeline.Oscillator []
        (\u ->
            start + total * (1 - abs (2 * u - 1))
        )


{-| Or make whatever kind of oscillator you need!

This takes a function which is given the progress of this oscillation as a `Float` between 0 and 1.

-}
interpolate : (Float -> Float) -> Oscillator
interpolate interp =
    Timeline.Oscillator [] interp



{- SPRITES -}


{-| -}
type alias Frames item =
    Timeline.Frames item


{-| -}
type alias Resting item =
    Timeline.Resting item


{-| Show a single `sprite`.
-}
frame : sprite -> Frames sprite
frame =
    Timeline.Single


{-| Show this `sprite` for a number of frames. Only really useful if you're using [`walk`](#walk) or [`cycle`](#cycle).
-}
hold : Int -> sprite -> Frames sprite
hold =
    Timeline.Hold


{-| Walk through a list of frames as we're transitioning to this state.
-}
walk : sprite -> List (Frames sprite) -> Frames sprite
walk =
    Timeline.Walk


{-| Here we have the same distinction of **transition** and **resting** that the rest of the library has.

With `framesWith` we can define the frames it takes to transition to this state, as well as what to do when we're in this state (maybe we want to loop through a number of frames when we're in this state).

-}
framesWith :
    { transition : Frames item
    , resting : Resting item
    }
    -> Frames item
framesWith cfg =
    Timeline.WithRest
        cfg.resting
        cfg.transition


{-| -}
type FramesPerSecond
    = FramesPerSecond Float


{-| -}
fps : Float -> FramesPerSecond
fps =
    FramesPerSecond


{-| While we're at this specific state, `cycle` through a list of frames at this `fps`.
-}
cycle : FramesPerSecond -> List (Frames sprite) -> Resting sprite
cycle (FramesPerSecond framesPerSecond) frames =
    let
        duration =
            Duration.seconds (toFloat (List.length frames) / framesPerSecond)
    in
    Timeline.Cycle (Timeline.Loop duration) frames


{-| Same as `cycle`, but only for `n` number of times.
-}
cycleN : Int -> FramesPerSecond -> List (Frames sprite) -> Resting sprite
cycleN n (FramesPerSecond framesPerSecond) frames =
    let
        duration =
            Duration.seconds (toFloat (List.length frames) / framesPerSecond)
    in
    Timeline.Cycle (Timeline.Repeat n duration) frames


{-| -}
step : Timeline state -> (state -> Frames sprite) -> sprite
step timeline lookup =
    let
        progress =
            Timeline.progress timeline

        currentFrameSet =
            lookup (current timeline)
    in
    if progress == 1 then
        restFrames currentFrameSet (Timeline.dwellingTime timeline)

    else
        stepFrames currentFrameSet progress


restFrames : Frames item -> Float -> item
restFrames currentFrameSet restingTimeMs =
    case currentFrameSet of
        Timeline.Single item ->
            item

        Timeline.Hold i item ->
            item

        Timeline.Walk start sprites ->
            let
                index =
                    totalFrames sprites - 1
            in
            getItemAtIndex index (Timeline.Single start) 0 sprites

        Timeline.WithRest (Timeline.Cycle period cycleFrameList) transitionFrames ->
            let
                len =
                    totalFrames cycleFrameList
            in
            case period of
                Timeline.Loop dur ->
                    let
                        iterationTimeMS =
                            Duration.inMilliseconds dur

                        progress =
                            wrapToUnit (restingTimeMs / iterationTimeMS)

                        targetIndex =
                            floor (progress * toFloat len)
                    in
                    getItemAtIndex targetIndex transitionFrames 0 cycleFrameList

                Timeline.Repeat n dur ->
                    let
                        iterationTimeMS =
                            Duration.inMilliseconds dur

                        iteration =
                            floor (restingTimeMs / iterationTimeMS)

                        progress =
                            if iteration >= n then
                                1

                            else
                                wrapToUnit (restingTimeMs / iterationTimeMS)

                        targetIndex =
                            floor (progress * toFloat len)
                    in
                    getItemAtIndex targetIndex transitionFrames 0 cycleFrameList


stepFrames : Frames item -> Float -> item
stepFrames currentFrameSet progress =
    case currentFrameSet of
        Timeline.Single item ->
            item

        Timeline.Hold i item ->
            item

        Timeline.Walk start sprites ->
            let
                frameCount =
                    totalFrames sprites

                index =
                    floor (progress * toFloat frameCount) - 1
            in
            getItemAtIndex index (Timeline.Single start) 0 sprites

        Timeline.WithRest _ newFrameSet ->
            stepFrames newFrameSet progress


totalFrames : List (Frames item) -> Int
totalFrames frames =
    List.foldl (\frm total -> total + frameSize frm) 0 frames


frameSize : Frames item -> Int
frameSize myFrame =
    case myFrame of
        Timeline.Single _ ->
            1

        Timeline.Hold i _ ->
            i

        Timeline.Walk i frames ->
            List.foldl (\frm total -> total + frameSize frm) 0 frames

        Timeline.WithRest _ newFrameSet ->
            frameSize newFrameSet


getItemAtIndex : Int -> Frames item -> Int -> List (Frames item) -> item
getItemAtIndex targetIndex transitionFrame currentIndex cycleList =
    case cycleList of
        [] ->
            lastFrame transitionFrame

        top :: remain ->
            case top of
                Timeline.Single item ->
                    if targetIndex == currentIndex then
                        item

                    else
                        getItemAtIndex targetIndex transitionFrame (currentIndex + 1) remain

                Timeline.Hold i item ->
                    if currentIndex <= targetIndex && currentIndex + i >= targetIndex then
                        item

                    else
                        getItemAtIndex targetIndex transitionFrame (currentIndex + i) remain

                Timeline.Walk item allFrames ->
                    let
                        frameCount =
                            totalFrames allFrames
                    in
                    if targetIndex < currentIndex + frameCount then
                        getItemAtIndex targetIndex transitionFrame currentIndex allFrames

                    else
                        getItemAtIndex targetIndex transitionFrame (currentIndex + frameCount) remain

                Timeline.WithRest _ frames ->
                    let
                        frameCount =
                            frameSize frames
                    in
                    if targetIndex < currentIndex + frameCount then
                        getItemAtIndex targetIndex transitionFrame currentIndex [ frames ]

                    else
                        getItemAtIndex targetIndex transitionFrame (currentIndex + frameCount) remain


lastFrame myFrame =
    case myFrame of
        Timeline.Single item ->
            item

        Timeline.Hold _ item ->
            item

        Timeline.Walk item remainingFrames ->
            case List.head (List.reverse remainingFrames) of
                Nothing ->
                    item

                Just last ->
                    lastFrame last

        Timeline.WithRest _ frames ->
            lastFrame frames


{-| An `Animator` knows how to read and write all the `Timelines` within your `Model`.

Here's an animator from the [Checkbox.elm example](https://github.com/mdgriffith/elm-animator/blob/master/examples/Checkbox.elm),

    animator : Animator.Animator Model
    animator =
        Animator.animator
            |> Animator.watching
                -- we tell the animator how
                -- to get the checked timeline using .checked
                .checked
                -- and we tell the animator how
                -- to update that timeline as well
                (\newChecked model ->
                    { model | checked = newChecked }
                )

Notice you could add any number of timelines to this animator.

**Note** — You likely only need one animator for a given project.

**Note 2** — Once we have an `Animator Model`, we have two more steps in order to set things up:

  - [create a _subscription_](#toSubscription)
  - [_update_ our model](#update)

-}
type alias Animator model =
    Timeline.Animator model


{-| -}
animator : Animator model
animator =
    Timeline.Animator (always False) (\now model -> model)


{-| `watching` will ensure that [`AnimationFrame`](https://package.elm-lang.org/packages/elm/browser/latest/Browser-Events#onAnimationFrame) is running when the animator is transformed into a [`subscription`](#toSubscription).

**Note** — It will actually make the animation frame subscription run all the time! At some point you'll probably want to optimize when the subscription runs, which means either using [`watchingWith`](#watchingWith) or `Animator.Css.watching`.

-}
watching :
    (model -> Timeline state)
    -> (Timeline state -> model -> model)
    -> Animator model
    -> Animator model
watching get set (Timeline.Animator isRunning updateModel) =
    Timeline.Animator
        -- always runs
        (always True)
        (\now model ->
            let
                newModel =
                    updateModel now model
            in
            set (Timeline.update now (get newModel)) newModel
        )


{-| `watchingWith` will allow you to have more control over when `AnimationFrame` runs.

The main thing you need to do here is capture which states are animated when they're **resting**.

Let's say we have a checkbox that, for whatever reason, we want to say is spinning forever when the value is `False`.

    animator : Animator.Animator Model
    animator =
        Animator.animator
            |> Animator.watchingWith .checked
                (\newChecked model ->
                    { model | checked = newChecked }
                )
                -- here is where we tell the animator that we still need
                -- AnimationFrame when the timeline has a current value of `False`
                (\checked ->
                    checked == False
                )

**Note** — if you're using `Animator.Css` to generate keyframes along with `Animator.Css.watching`, you don't need to worry about this.

-}
watchingWith :
    (model -> Timeline state)
    -> (Timeline state -> model -> model)
    -> (state -> Bool)
    -> Animator model
    -> Animator model
watchingWith get set eventIsRestable (Timeline.Animator isRunning updateModel) =
    Timeline.Animator
        (\model ->
            -- if we're already running, skip
            if isRunning model then
                True

            else
                let
                    timeline =
                        get model
                in
                if Timeline.needsUpdate timeline then
                    True

                else
                    eventIsRestable (Timeline.current timeline)
        )
        (\now model ->
            let
                newModel =
                    updateModel now model
            in
            set (Timeline.update now (get newModel)) newModel
        )


{-| Convert an `Animator` to a subscription.

This is where the animator will decide if a running animation needs another frame or not.

    subscriptions model =
        Animator.toSubscription Tick model animator

-}
toSubscription : (Time.Posix -> msg) -> model -> Animator model -> Sub msg
toSubscription toMsg model (Timeline.Animator isRunning _) =
    if isRunning model then
        Browser.Events.onAnimationFrame
            toMsg

    else
        Sub.none


{-| When new messages come in, we then need to update our model. This looks something like this:

    type Msg
        = Tick Time.Posix

    update msg model =
        case msg of
            Tick newTime ->
                ( Animator.update newTime animator model
                , Cmd.none
                )

And voilà, we can begin animating!

**Note** — To animate more things, all you need to do is add a new `with` to your `Animator`.

-}
update : Time.Posix -> Animator model -> model -> model
update newTime (Timeline.Animator _ updateModel) model =
    updateModel newTime model


{-| If you're creating something like a game, you might want to update your `Timelines` manually instead of using an `Animator`.

This will allow you to do whatever calculations you need while updating each `Timeline`.

**Note** — You'll have to take care of subscribing to `Browser.Events.onAnimationFrame`.

-}
updateTimeline : Time.Posix -> Timeline state -> Timeline state
updateTimeline =
    Timeline.update

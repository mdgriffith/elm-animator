module Animator.Timeline exposing
    ( Timeline, init
    , to
    , Duration, ms
    , update, isRunning
    , interrupt, queue
    , Step, wait, transitionTo
    , scale, delay
    , current, previous, upcoming, upcomingWith, arrived, arrivedAt, arrivedAtWith
    )

{-|


# Getting started

`elm-animator` is about taking pieces of your model, turning them into **Timelines** of values, and animate between their states

@docs Timeline, init


# Transitioning to a new state

Now that we have a `Timeline` set up, we likely want to set a new **value**.

In order to do that we need to specify both —

  - the new state we want to be in
  - a `Duration` for how long this transition should take.

@docs to

@docs Duration, ms

@docs update, isRunning


# Interruptions and Queueing

In some more **advanced** cases you might want to define a _series_ of states to animate through instead of just going to one directly.

    Timeline.interrupt
        [ Timeline.wait (Timeline.millis 300)

        -- after waiting 300 milliseconds,
        -- start transitioning to a new state, Griffyndor
        -- Take 1 whole second to make the transition
        , Timeline.transitionTo (Timeline.seconds 1) Griffyndor

        -- Once we've arrived at Griffyndor,
        -- immediately start transitioning to Slytherin
        -- and take half a second to make the transition
        , Timeline.transitionTo (Timeline.seconds 0.5) Slytherin
        ]

@docs interrupt, queue

@docs Step, wait, transitionTo

@docs scale, delay


# Reading the timeline

You might be wondering, 'How do we get our value "out" of a `Timeline`?'

Well, we can ask the `Timeline` all sorts of questions.

@docs current, previous, upcoming, upcomingWith, arrived, arrivedAt, arrivedAtWith

-}

import Duration
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
        , delay = Duration.milliseconds 0
        , scale = 1
        , events =
            Timeline.Timetable []
        , queued = Nothing
        , interruption = []
        , running = True
        }


{-| Delay the events of a timeline.

This is generally used in your view function to add a bit of variety when animating multiple elements.

        Animator.move (Animator.delay (Animator.millis 200) timeline) <|
            \state ->
                if state then
                    Animator.at 0

                else
                    Animator.at 1

This has a maximum value of 5 seconds.

If you need a longer delay, it's likely you want to create a separate timeline.

-}
delay : Duration -> Timeline state -> Timeline state
delay dur (Timeline.Timeline details) =
    Timeline.Timeline
        { details | delay = Time.maxDuration (Duration.milliseconds 5000) (Time.expand details.delay (Time.positiveDuration dur)) }


{-| Speedup or slowdown a timeline.

    0.5 -> half speed
    1.0 -> normal
    2.0 -> twice as fast

**Note** - 0.1 is the lowest number allowed, and 5 is the highest.

This is generally used in your view function to add a bit of variety when animating multiple elements.

-}
scale : Float -> Timeline state -> Timeline state
scale factor (Timeline.Timeline details) =
    Timeline.Timeline
        { details | scale = min 5 (max 0.1 factor) }


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
ms : Float -> Duration
ms =
    Duration.milliseconds


{-| -}
type Step state
    = Wait Duration
    | TransitionTo Duration state


{-| -}
transitionTo : Duration -> state -> Step state
transitionTo =
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
                        case initializeSchedule (ms 0) steps of
                            Nothing ->
                                tl.queued

                            Just ( schedule, otherSteps ) ->
                                Just (List.foldl stepsToEvents schedule otherSteps)

                    Just queued ->
                        Just (List.foldl stepsToEvents queued steps)
        }


{-| Go to a new state!

You'll need to specify a `Duration` as well. Try starting with `Animator.quickly` and adjust up or down as necessary.

-}
to : Duration -> state -> Timeline state -> Timeline state
to duration ev timeline =
    interrupt [ transitionTo duration ev ] timeline


{-| Interrupt what's currently happening with a new list.
-}
interrupt : List (Step state) -> Timeline state -> Timeline state
interrupt steps (Timeline.Timeline tl) =
    Timeline.Timeline
        { tl
            | running = True
            , interruption =
                case initializeSchedule (ms 0) steps of
                    Nothing ->
                        tl.interruption

                    Just ( schedule, otherSteps ) ->
                        -- **NOTE** - if we recieve a new interruption, we throw away the existing one!
                        -- This was leading to issues when the same event was added to the `interrupted` queue
                        -- multiple times in before being scheduled.
                        -- So, I imagine it does make sense to dedup these
                        -- But does it ALWAYS make sense to replace the currently scheduled interruption?
                        [ List.foldl stepsToEvents schedule otherSteps ]
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
stepsToEvents currentStep (Timeline.Schedule delayTime startEvent events) =
    case events of
        [] ->
            case currentStep of
                Wait waiting ->
                    Timeline.Schedule
                        delayTime
                        (Timeline.extendEventDwell waiting startEvent)
                        events

                TransitionTo dur checkpoint ->
                    Timeline.Schedule
                        delayTime
                        startEvent
                        [ Timeline.Event dur checkpoint Nothing ]

        (Timeline.Event durationTo recentEvent maybeDwell) :: remaining ->
            case currentStep of
                Wait dur ->
                    Timeline.Schedule
                        delayTime
                        startEvent
                        (Timeline.Event durationTo recentEvent (Timeline.addToDwell dur maybeDwell) :: remaining)

                TransitionTo dur checkpoint ->
                    if checkpoint == recentEvent then
                        Timeline.Schedule
                            delayTime
                            startEvent
                            (Timeline.Event durationTo recentEvent (Timeline.addToDwell dur maybeDwell) :: remaining)

                    else
                        Timeline.Schedule
                            delayTime
                            startEvent
                            (Timeline.Event dur checkpoint Nothing :: events)


{-| -}
type alias Event state =
    Timeline.Event state


{-| -}
type alias Schedule state =
    Timeline.Schedule state


{-| If you're creating something like a game, you might want to update your `Timelines` manually instead of using an `Animator`.

This will allow you to do whatever calculations you need while updating each `Timeline`.

**Note** — You'll have to take care of subscribing to `Browser.Events.onAnimationFrame`.

-}
update : Time.Posix -> Timeline state -> Timeline state
update =
    Timeline.update


{-| Does this timeline have upcoming events?

**Note** this is only useful if you're not using a `Animator.Watcher`

-}
isRunning : Timeline state -> Bool
isRunning (Timeline.Timeline tl) =
    tl.running

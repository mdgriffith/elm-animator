module Animator.Timeline exposing
    ( Timeline, init
    , to
    , update, isRunning, hasChanges
    , interrupt, queue
    , Step, wait, transitionTo
    , scale, delay, Duration
    , current, previous, upcoming, upcomingWith, arrived, arrivedAt, arrivedAtWith
    , progress
    )

{-|


# Getting started

Here's how to keep a timeline in your model and update it:

    import Animator
    import Animator.Timeline as Timeline
    import Browser.Events
    import Time

    type alias Model =
        { visible : Timeline.Timeline Bool }

    init : Model
    init =
        { visible = Timeline.init False }

    type Msg
        = Tick Time.Posix
        | Show

    subscriptions : Model -> Sub Msg
    subscriptions model =
        if Timeline.isRunning model.visible then
            Browser.Events.onAnimationFrame Tick

        else
            Sub.none

    update : Msg -> Model -> ( Model, Cmd Msg )
    update msg model =
        case msg of
            Tick now ->
                ( { model | visible = Timeline.update now model.visible }
                , Cmd.none
                )

            Show ->
                ( { model | visible = Timeline.to (Animator.ms 300) True model.visible }
                , Cmd.none
                )

@docs Timeline, init


# Transitioning to a new state

Now that we have a `Timeline` set up, we likely want to set a new **value**.

In order to do that we need to specify both —

  - the new state we want to be in
  - a `Duration` for how long this transition should take.

@docs to

@docs update, isRunning, hasChanges


# Interruptions and Queueing

In some more **advanced** cases you might want to define a _series_ of states to animate through instead of just going to one directly.

    Timeline.interrupt
        [ Timeline.wait (Animator.ms 300)

        -- after waiting 300 milliseconds,
        -- start transitioning to a new state, Griffyndor
        -- Take 1 whole second to make the transition
        , Timeline.transitionTo (Animator.ms 1000) Griffyndor

        -- Once we've arrived at Griffyndor,
        -- immediately start transitioning to Slytherin
        -- and take half a second to make the transition
        , Timeline.transitionTo (Animator.ms 500) Slytherin
        ]
        timeline

@docs interrupt, queue

@docs Step, wait, transitionTo

@docs scale, delay, Duration


# Reading the timeline

You might be wondering, 'How do we get our value "out" of a `Timeline`?'

Well, we can ask the `Timeline` all sorts of questions.

@docs current, previous, upcoming, upcomingWith, arrived, arrivedAt, arrivedAtWith

@docs progress

-}

import InternalAnim.Duration as Duration
import InternalAnim.Quantity as Quantity
import InternalAnim.Time as Time
import InternalAnim.Timeline as Timeline
import Time


{-| A timeline of `state` values.

Behind the scenes this is roughly a list of states and the times that they should occur!

-}
type alias Timeline state =
    Timeline.Timeline state


{-| A duration shared with `Animator.Duration`. Construct one with `Animator.ms`.
-}
type alias Duration =
    Time.Duration


{-| Create a timeline with an initial `state`.

So, if you previously had a `Bool` in your model:

    type alias Model = { checked : Bool }

    -- created via
    { checked = False }

You could replace that with a `Timeline.Timeline Bool`:

    type alias Model = { checked : Timeline.Timeline Bool }

    -- created via
    { checked = Timeline.init False }

-}
init : state -> Timeline state
init first =
    let
        epoch =
            Time.absolute (Time.millisToPosix 0)
    in
    Timeline.Timeline
        { initial = first
        , initialStartedAt = Nothing
        , now = epoch
        , updatedAt = epoch
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

        Animator.Value.float (Animator.Timeline.delay (Animator.ms 200) timeline) <|
            \state ->
                if state then
                    Animator.Value.to 0

                else
                    Animator.Value.to 1

Delays add together, negative additions are ignored, and the total is capped at
5 seconds.

If you need a longer delay, it's likely you want to create a separate timeline.

-}
delay : Duration -> Timeline state -> Timeline state
delay dur (Timeline.Timeline details) =
    Timeline.Timeline
        { details
            | delay =
                Duration.milliseconds
                    (min (Duration.inMilliseconds Timeline.maxDelay)
                        (Duration.inMilliseconds details.delay + max 0 (Duration.inMilliseconds dur))
                    )
        }


{-| Scale durations when pending steps are scheduled by the next `update`.

    0.5 -> Animations take half as much time
    1.0 -> normal
    2.0 -> Animations take twice as long

**Note** - 0.1 is the lowest number allowed, and 5 is the highest.

Set this on the model's timeline before calling `update`. It does not retime
events that have already been scheduled, so applying it only in a view does not
change their playback speed.

-}
scale : Float -> Timeline state -> Timeline state
scale factor (Timeline.Timeline details) =
    Timeline.Timeline
        { details | scale = min 5 (max 0.1 factor) }


{-| The proportion (number between 0 and 1) of progress between the last state and the new one.

Once we arrive at a new state, this value will be 1 until we start another transition.

-}
progress : Timeline state -> Float
progress =
    Timeline.progress


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


{-| Subtly different than [`current`](#current), this will provide the new state as soon as the transition has _finished_.

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

Use `arrivedAt` before updating the timeline to detect arrivals after its current
time and at or before the new tick. An arrival is not reported again on the next
tick. Destinations canceled by an interruption are not reported.

    Tick time ->
        if Animator.Timeline.arrivedAt MyState time model.timeline then
            --...do something special

-}
arrivedAt : state -> Time.Posix -> Timeline state -> Bool
arrivedAt state =
    Timeline.arrivedAt ((==) state)


{-| -}
arrivedAtWith : (state -> Bool) -> Time.Posix -> Timeline state -> Bool
arrivedAtWith =
    Timeline.arrivedAt


{-| Get the previous `state` on this timeline.

As you'll see in the [Loading example](https://github.com/mdgriffith/elm-animator/blob/master/examples/src/Loading.elm), it means we can use `previous` to refer to data that we've already "deleted" or set to `Nothing`.

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


{-| -}
upcomingWith : (state -> Bool) -> Timeline state -> Bool
upcomingWith =
    Timeline.upcoming



-- future : Timeline state -> List ( TIme.Posix, state )


{-| -}
type Step state
    = Wait Time.Duration
    | TransitionTo Time.Duration state


{-| -}
transitionTo : Duration -> state -> Step state
transitionTo =
    TransitionTo


{-| A list containing only waits has no destination and does not create a schedule.
-}
wait : Duration -> Step state
wait =
    Wait


{-| Wait until the current timeline is **finished** and then continue with these new steps.

Pending steps are scheduled on the next `update`.

-}
queue : List (Step state) -> Timeline state -> Timeline state
queue steps (Timeline.Timeline tl) =
    Timeline.Timeline
        { tl
            | running = True
            , queued =
                case tl.queued of
                    Nothing ->
                        -- This consumes the first `wait` and adds it to the schedule as the initial delay
                        -- It also consumes the first real event
                        case initializeSchedule (Duration.milliseconds 0) steps of
                            Nothing ->
                                tl.queued

                            Just ( schedule, otherSteps ) ->
                                Just (List.foldl stepsToEvents schedule otherSteps)

                    Just queued ->
                        Just (List.foldl stepsToEvents queued steps)
        }


{-| Interrupt the existing schedule on the next `update`.
-}
to : Duration -> state -> Timeline state -> Timeline state
to duration ev timeline =
    interrupt [ transitionTo duration ev ] timeline


{-| If several interruptions are requested before the next `update`, the latest
nonempty schedule wins.
An initial `wait` lets existing motion continue until the replacement begins.
-}
interrupt : List (Step state) -> Timeline state -> Timeline state
interrupt steps (Timeline.Timeline tl) =
    Timeline.Timeline
        { tl
            | running = True
            , interruption =
                case initializeSchedule (Duration.milliseconds 0) steps of
                    Nothing ->
                        tl.interruption

                    Just ( schedule, otherSteps ) ->
                        -- **NOTE** - if we recieve a new interruption, we throw away the existing one!
                        -- This was leading to issues when the same event was added to the `interrupted` queue
                        -- multiple times in before being scheduled.
                        [ List.foldl stepsToEvents schedule otherSteps ]
        }


{-| -}
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
                        (Timeline.Event durationTo
                            recentEvent
                            (Timeline.addToDwell dur maybeDwell)
                            :: remaining
                        )

                TransitionTo dur checkpoint ->
                    if checkpoint == recentEvent then
                        Timeline.Schedule
                            delayTime
                            startEvent
                            (Timeline.Event durationTo
                                recentEvent
                                (Timeline.addToDwell dur maybeDwell)
                                :: remaining
                            )

                    else
                        Timeline.Schedule
                            delayTime
                            startEvent
                            (Timeline.Event dur checkpoint Nothing :: events)


{-| -}
type alias Schedule state =
    Timeline.Schedule state


{-| Call with the timestamp from `Browser.Events.onAnimationFrame` while
`isRunning`, or from an existing game loop. Check `arrivedAt` before updating if
you need to detect arrivals during this tick.
-}
update : Time.Posix -> Timeline state -> Timeline state
update =
    Timeline.update


{-| Use this to control your animation-frame subscription. CSS resting loops can
continue in the browser even when this returns `False`.
-}
isRunning : Timeline state -> Bool
isRunning (Timeline.Timeline tl) =
    tl.running


{-| `True` when scheduling requests are waiting for the next `update`. Becomes
`False` once they are scheduled, even if the animation is still running.
Use `isRunning` for animation-frame subscriptions.
-}
hasChanges : Timeline state -> Bool
hasChanges (Timeline.Timeline tl) =
    case tl.queued of
        Nothing ->
            not (List.isEmpty tl.interruption)

        Just _ ->
            True

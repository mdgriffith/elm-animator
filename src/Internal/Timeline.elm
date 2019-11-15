module Internal.Timeline exposing
    ( Timeline(..), Occurring(..)
    , Schedule(..), Event(..)
    , Interpolator, Promoter, TimelineDetails, foldp, needsUpdate, rewrite, update
    )

{-|

@docs Timeline, Occurring

@docs Schedule, Event

-}

import Internal.Time as Time
import Quantity
import Time


{-| A list of events that haven't been added to the schedule yet.
-}
type Schedule event
    = Schedule (List (Event event))


{-| -}
type Event event
    = Event Time.Duration event


{-| -}
type Timeline event
    = Timeline (TimelineDetails event)


type alias TimelineDetails event =
    { initial : event
    , start : Time.Absolute
    , now : Time.Absolute
    , events : List (Occurring event)
    , queued : Maybe (Schedule event)
    , running : Bool
    }


{-| started at list of events that were scheduled
-}
type Timetable event
    = Timetable Time.Absolute (List (Occurring event))


type Occurring event
    = Occurring event Time.Absolute


filterMapOccurring : (event -> Maybe newEvent) -> Occurring event -> Maybe (Occurring newEvent)
filterMapOccurring fn (Occurring ev time) =
    case fn ev of
        Nothing ->
            Nothing

        Just newEv ->
            Just (Occurring newEv time)


{-| -}
rewrite : newEvent -> Timeline event -> (event -> Maybe newEvent) -> Timeline newEvent
rewrite newStart (Timeline tl) newLookup =
    Timeline
        { initial = newStart
        , start = tl.start
        , now = tl.now
        , running = tl.running
        , events =
            List.filterMap (filterMapOccurring newLookup) tl.events
        , queued = Nothing
        }


{-| -}
needsUpdate : Timeline event -> Bool
needsUpdate (Timeline timeline) =
    (timeline.queued /= Nothing)
        || timeline.running


{-| -}
update : Time.Posix -> Timeline event -> Timeline event
update now (Timeline timeline) =
    Timeline
        { timeline
            | now = Time.absolute now
            , start =
                if Quantity.Quantity 0 == timeline.start then
                    Time.absolute now

                else
                    timeline.start
            , events =
                case timeline.queued of
                    Nothing ->
                        timeline.events

                    Just queuedSchedule ->
                        enqueue timeline (Time.absolute now) queuedSchedule
            , queued = Nothing
        }


{-| -}
enqueue : TimelineDetails events -> Time.Absolute -> Schedule events -> List (Occurring events)
enqueue timeline now (Schedule queued) =
    let
        finalEvent =
            List.reverse timeline.events
                |> List.head
    in
    case finalEvent of
        Nothing ->
            Occurring timeline.initial now
                :: (List.foldl toOccurring ( now, [] ) queued
                        |> Tuple.second
                        |> List.reverse
                   )

        Just (Occurring lastEvent lastEventTime) ->
            let
                startNewEventsAt =
                    Time.latest lastEventTime now

                newEvents =
                    List.foldl toOccurring ( startNewEventsAt, [] ) queued
                        |> Tuple.second
                        |> List.reverse
            in
            if Time.thisAfterThat now lastEventTime then
                timeline.events ++ Occurring lastEvent now :: newEvents

            else
                timeline.events ++ newEvents


toOccurring : Event event -> ( Time.Absolute, List (Occurring event) ) -> ( Time.Absolute, List (Occurring event) )
toOccurring (Event duration event) ( now, events ) =
    let
        occursAt =
            Time.advanceBy duration now
    in
    ( occursAt, Occurring event occursAt :: events )


{-| Ok, we have a few subtle concepts here.

1.  `event` is the event type provided by the user.
    _Example_ `ShowModal`

2.  `anchor` is the direct value that the event is mapped to.
    _Example_ {x,y} or {angle}
    This is essentially the communicated constraint that is desired.

3.  `motion` is the anchor plus additional data informed by previous and upcoming `anchors` as well as timestamps.
    _Example_ {x,y,velocity}

And we have some type aliases to capture how to create each one of those values.

    `Promoter` is a function that transforms an `anchor` into a `motion`.

    `Interpolator` is a function that maps linearly between two `motions`.

-}
foldp : (event -> anchor) -> Promoter anchor motion -> Interpolator motion -> Timeline event -> motion
foldp lookup promote interp (Timeline timeline) =
    .state <|
        List.foldl
            (\_ cursor ->
                case cursor.events of
                    [] ->
                        cursor

                    (Occurring target targetTime) :: remaining ->
                        if cursor.done then
                            cursor

                        else if Time.thisAfterThat timeline.now targetTime || Time.equal timeline.now targetTime then
                            -- happened in the past,
                            -- capture a snapshot of what happens directly on that event
                            let
                                lookAheadAnchor =
                                    List.head remaining
                                        |> Maybe.map (applyLookup lookup)

                                currentAnchor =
                                    lookup target
                            in
                            { state = promote cursor.previousAnchor currentAnchor targetTime lookAheadAnchor
                            , events = remaining
                            , previousEventTime = targetTime
                            , previousAnchor = Just ( currentAnchor, targetTime )
                            , done = False
                            }

                        else if Time.thisBeforeThat timeline.now targetTime then
                            -- This transition is happenind right now.
                            -- Tnterpolate to this exact time and flag as done.
                            let
                                lookAheadAnchor =
                                    List.head remaining
                                        |> Maybe.map (applyLookup lookup)

                                currentAnchor =
                                    lookup target

                                promotedTarget =
                                    promote cursor.previousAnchor (lookup target) targetTime lookAheadAnchor

                                progress =
                                    Time.progress cursor.previousEventTime targetTime timeline.now
                            in
                            -- happened in the past,
                            -- capture a snapshot of what happens directly on that event
                            { state = interp cursor.state promotedTarget progress
                            , events = remaining
                            , previousEventTime = timeline.now
                            , previousAnchor = Just ( currentAnchor, timeline.now )
                            , done = True
                            }

                        else
                            -- shouldn't be reachable...
                            -- Not sure how to guard against that though.
                            cursor
            )
            { state = promote Nothing (lookup timeline.initial) timeline.start Nothing
            , events = timeline.events
            , previousEventTime = timeline.start
            , previousAnchor = Nothing
            , done = List.isEmpty timeline.events
            }
            timeline.events


applyLookup lookup (Occurring event time) =
    ( lookup event, time )


type alias Progress =
    Float


type alias Promoter value state =
    Maybe ( value, Time.Absolute ) -> value -> Time.Absolute -> Maybe ( value, Time.Absolute ) -> state


type alias Interpolator entity =
    entity -> entity -> Progress -> entity

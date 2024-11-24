module Sequencing exposing (suite)

{-| This test suite is for testing that Move.Sequences are generated correctly
-}

import Expect exposing (Expectation, FloatingPointTolerance(..))
import Fuzz exposing (Fuzzer)
import InternalAnim.Move as Move
import InternalAnim.Time as Time
import InternalAnim.Transition as Transition
import InternalAnim.Units as Units
import Test


transition =
    { standard = Transition.standard
    , linear = Transition.linear
    , wobble =
        Transition.wobble
            { wobble = 1
            , quickness = 0
            }
    }


getGroundTruth : Time.Absolute -> Time.Absolute -> Time.Absolute -> Move.Move Float -> Move.State -> Move.State
getGroundTruth start now end movement initial =
    let
        progress =
            Time.progress start end now
    in
    Move.at progress start end movement initial


getSequenceState : Time.Absolute -> Time.Absolute -> List (Move.Sequence Float) -> Move.State -> Move.State
getSequenceState startedAt now sequenceList initial =
    List.foldl
        (\seq ( state, isFound ) ->
            if isFound then
                ( state, isFound )

            else
                ( onSequence startedAt now seq state
                , True
                )
        )
        ( initial, False )
        sequenceList
        |> Tuple.first


onSequence : Time.Absolute -> Time.Absolute -> Move.Sequence Float -> Move.State -> Move.State
onSequence sequenceStartedAt now (Move.Sequence repeat seqDelay seqDuration steps) initial =
    List.foldl
        (\(Move.Step stepDur stepTransition stepTarget) ( startedAt, state ) ->
            let
                stepEnd =
                    Time.advanceBy stepDur startedAt

                progress =
                    Time.progress startedAt stepEnd now
            in
            ( stepEnd
            , Transition.atX progress stepTransition
                |> Move.denormalize startedAt
                    stepEnd
                    state.position
                    stepTarget
                |> unwrapPosition
            )
        )
        ( sequenceStartedAt
        , initial
            |> unwrapPosition
        )
        steps
        |> Tuple.second
        |> wrapPosition


wrapPosition : { position : Float, velocity : Units.PixelsPerSecond } -> Move.State
wrapPosition { position, velocity } =
    { position = Units.pixels position
    , velocity = velocity
    }


unwrapPosition :
    Move.State
    ->
        { position : Float
        , velocity : Units.PixelsPerSecond
        }
unwrapPosition state =
    { position = Units.inPixels state.position
    , velocity = state.velocity
    }


type alias Interruption =
    { start : Time.Absolute
    , end : Time.Absolute
    , endPosition : Float
    , transition : Transition.Transition
    }


realtime :
    { start : Time.Absolute
    , now : Time.Absolute
    , end : Time.Absolute
    , position : { start : Float, end : Float }
    , transition : Transition.Transition
    , interruptions : List Interruption
    }
    -> Move.State
realtime options =
    let
        movement =
            Move.toWith options.transition options.position.end

        transitionTime =
            case options.interruptions of
                [] ->
                    options.now

                interruption :: _ ->
                    interruption.start
    in
    Move.init (Move.to options.position.start)
        |> getGroundTruth options.start transitionTime options.end movement
        |> interruptRealTime options.now options.interruptions


interruptRealTime : Time.Absolute -> List Interruption -> Move.State -> Move.State
interruptRealTime now interruptList state =
    case interruptList of
        [] ->
            state

        interruption :: rest ->
            let
                movement =
                    Move.toWith interruption.transition interruption.endPosition

                transitionTime =
                    case rest of
                        [] ->
                            now

                        next :: _ ->
                            next.start
            in
            state
                |> getGroundTruth interruption.start transitionTime interruption.end movement
                |> interruptRealTime now rest


{-| Of note for sequences!
-}
bySequence :
    { start : Time.Absolute
    , now : Time.Absolute
    , end : Time.Absolute
    , position : { start : Float, end : Float }
    , transition : Transition.Transition
    , interruptions : List Interruption
    }
    -> Move.State
bySequence options =
    let
        movement =
            Move.toWith options.transition options.position.end

        initial =
            Move.init (Move.to options.position.start)

        sequences =
            let
                _ =
                    Debug.log "Sequences" "---"
            in
            -- Note                                  v-----------v this is when we start generating the sequence
            Move.sequences options.start options.end options.start options.end movement []
                |> interrupt options.now options.interruptions
                |> List.map (Debug.log "  Seq")
    in
    getSequenceState options.start options.now sequences initial


interrupt : Time.Absolute -> List Interruption -> List (Move.Sequence Float) -> List (Move.Sequence Float)
interrupt now interruptList sequences =
    case interruptList of
        [] ->
            sequences

        interruption :: rest ->
            let
                movement =
                    Move.toWith interruption.transition interruption.endPosition

                newSequences =
                    Move.sequences
                        interruption.start
                        interruption.end
                        interruption.start
                        -- now
                        interruption.end
                        movement
                        sequences
            in
            interrupt now rest newSequences


suite : Test.Test
suite =
    Test.describe "Realtime & Sync matching"
        [ basicMovement
        , interruptions
        ]


basicMovement : Test.Test
basicMovement =
    Test.describe "Move sequences"
        [ Test.test "Linear" <|
            \_ ->
                let
                    groundTruth =
                        realtime
                            { start = Time.millis 0
                            , now = Time.millis 500
                            , end = Time.millis 1000
                            , transition = Transition.linear
                            , position = { start = 0, end = 1000 }
                            , interruptions = []
                            }

                    sequenceState =
                        bySequence
                            { start = Time.millis 0
                            , now = Time.millis 500
                            , end = Time.millis 1000
                            , transition = Transition.linear
                            , position = { start = 0, end = 1000 }
                            , interruptions = []
                            }
                in
                Expect.equal
                    groundTruth.position
                    sequenceState.position
        , Test.test "Standard" <|
            \_ ->
                let
                    groundTruth =
                        realtime
                            { start = Time.millis 0
                            , now = Time.millis 500
                            , end = Time.millis 1000
                            , transition = Transition.standard
                            , position = { start = 0, end = 1000 }
                            , interruptions = []
                            }

                    sequenceState =
                        bySequence
                            { start = Time.millis 0
                            , now = Time.millis 500
                            , end = Time.millis 1000
                            , transition = Transition.standard
                            , position = { start = 0, end = 1000 }
                            , interruptions = []
                            }
                in
                Expect.equal
                    groundTruth.position
                    sequenceState.position
        ]


interruptions : Test.Test
interruptions =
    Test.describe "Interruptions "
        [ Test.test "Linear" <|
            \_ ->
                let
                    options =
                        { start = Time.millis 0
                        , now = Time.millis 1000
                        , end = Time.millis 1000
                        , transition = Transition.linear
                        , position = { start = 0, end = 1000 }
                        , interruptions =
                            [ { start = Time.millis 500
                              , end = Time.millis 1500
                              , endPosition = 2000
                              , transition = Transition.linear
                              }
                            ]
                        }

                    groundTruth =
                        realtime options

                    sequenceState =
                        bySequence options
                in
                Expect.equal
                    (Debug.log "Ground Truth" groundTruth.position)
                    sequenceState.position
        ]

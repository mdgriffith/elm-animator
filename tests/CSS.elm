module CSS exposing (frames)

import Animator
import Duration
import Expect exposing (Expectation, FloatingPointTolerance(..))
import Fuzz exposing (Fuzzer, float, int, list, string)
import Internal.Estimation as Estimate
import Internal.Interpolate as Interpolate
import Internal.Timeline as Timeline
import Pixels
import Quantity
import Test exposing (..)
import Time


type Event
    = Starting
    | One
    | Two
    | Three
    | Four
    | Five
    | Unreachable


toVals event =
    case event of
        Starting ->
            0

        One ->
            1

        Two ->
            2

        Three ->
            3

        Four ->
            4

        Five ->
            5

        Unreachable ->
            -1


toMoving event =
    case event of
        Starting ->
            Animator.wave 0 100
                |> Animator.loop (Animator.millis 1999)

        One ->
            Animator.at 1

        Two ->
            Animator.at 2

        Three ->
            Animator.at 3

        Four ->
            Animator.at 4

        Five ->
            Animator.at 5

        Unreachable ->
            Animator.at -1


framePercent (Timeline.Frame percent val) =
    percent


frames =
    describe "Frame Capturing"
        [ test "Frames generate a 0% frame" <|
            \_ ->
                let
                    timeline =
                        Animator.init One
                            |> Timeline.update (Time.millisToPosix 0)

                    val =
                        Timeline.capture 60 toMoving Interpolate.moving timeline

                    first =
                        List.head val.frames
                            |> Maybe.map framePercent
                in
                Expect.equal
                    first
                    (Just 0)
        , test "Frames generate a 100% frame" <|
            \_ ->
                let
                    timeline =
                        Animator.init One
                            |> Timeline.update (Time.millisToPosix 0)

                    val =
                        Timeline.capture 60 toMoving Interpolate.moving timeline

                    first =
                        List.head (List.reverse val.frames)
                            |> Maybe.map framePercent
                in
                Expect.equal
                    first
                    (Just 1)
        , test "Transitioning" <|
            \_ ->
                let
                    timeline =
                        Animator.init Starting
                            |> Timeline.update (Time.millisToPosix 0)
                            |> Animator.to (Animator.seconds 1) One
                            -- NOTE* possible schduling bug
                            -- scheduling an event
                            |> Timeline.update (Time.millisToPosix 1)

                    resultFrames =
                        Timeline.capture 60 toVals Interpolate.linearly timeline
                in
                Expect.equal
                    (List.length resultFrames.frames)
                    -- NOTE, this should probably be 60
                    -- but we're mostly concerned with it giving us any frames at all.
                    62
        , test "Transitioning to two events" <|
            \_ ->
                let
                    timeline =
                        Animator.init Starting
                            |> Timeline.update (Time.millisToPosix 0)
                            |> Animator.queue
                                [ Animator.event (Animator.seconds 1) One
                                , Animator.event (Animator.seconds 1) Two
                                ]
                            -- NOTE* possible schduling bug
                            -- should be abl to schedule an event at 0
                            |> Timeline.update (Time.millisToPosix 1)

                    resultFrames =
                        Timeline.capture 60 toVals Interpolate.linearly timeline
                in
                Expect.all
                    [ \frms ->
                        Expect.equal
                            (List.length frms.frames)
                            -- 120 frames, plus one to start and one to end.
                            121
                    , \frms ->
                        Expect.equal
                            (List.head frms.frames)
                            -- We start at a value of 0
                            (Just (Timeline.Frame 0 0))
                    , \frms ->
                        Expect.equal
                            (List.head (List.reverse frms.frames))
                            -- We end at a value of 2
                            (Just (Timeline.Frame 1 2))
                    ]
                    resultFrames
        , test "Transitioning to two events, interruption" <|
            \_ ->
                let
                    timeline =
                        Animator.init Starting
                            |> Timeline.update (Time.millisToPosix 0)
                            |> Animator.queue
                                [ Animator.event (Animator.seconds 1) One
                                , Animator.event (Animator.seconds 1) Two
                                ]
                            -- NOTE* possible schduling bug
                            -- scheduling an event
                            |> Timeline.update (Time.millisToPosix 1)
                            |> Animator.to (Animator.seconds 1) Three
                            |> Timeline.updateNoGC (Time.millisToPosix 500)

                    resultFrames =
                        Timeline.capture 60 toVals Interpolate.linearly timeline
                in
                Expect.equal
                    (List.length resultFrames.frames)
                    61
        ]

module InitialResting exposing (suite)

import Animator as Anim
import Animator.Timeline as Timeline
import Animator.Transition as Transition
import Dict
import Expect
import Support.Css as Css
import Test exposing (Test, describe, test)
import Time


suite : Test
suite =
    describe "Initial resting animation continuity"
        [ test "The first interruption samples the initial loop relative to the first tick" <|
            \_ ->
                initial
                    |> interruptAt 10537
                    |> render loop
                    |> startsAt 0.5
        , test "Later iterations retain the original loop phase" <|
            \_ ->
                initial
                    |> interruptAt 12537
                    |> render loop
                    |> startsAt 0.5
        , test "A finite initial sequence holds its final value before interruption" <|
            \_ ->
                initial
                    |> interruptAt 12537
                    |> render steps
                    |> startsAt 1
        , test "A queued initial wait lets the resting loop continue until departure" <|
            \_ ->
                initial
                    |> Timeline.update (Time.millisToPosix 10537)
                    |> Timeline.queue
                        [ Timeline.wait (Anim.ms 250)
                        , Timeline.transitionTo (Anim.ms 1000) True
                        ]
                    |> Timeline.update (Time.millisToPosix 10537)
                    |> render loop
                    |> startsAt 0.75
        , test "Ordinary idle ticks preserve the initial loop's CSS identity" <|
            \_ ->
                Expect.equal
                    (render loop initial)
                    (initial |> Timeline.update (Time.millisToPosix 10537) |> render loop)
        , test "A wait queued before the first tick runs the initial resting animation" <|
            \_ ->
                Timeline.init False
                    |> Timeline.queue
                        [ Timeline.wait (Anim.ms 500)
                        , Timeline.transitionTo (Anim.ms 1000) True
                        ]
                    |> Timeline.update (Time.millisToPosix 10037)
                    |> render loop
                    |> startsAt 0.5
        , test "A first tick at zero is an established anchor, not an uninitialized clock" <|
            \_ ->
                Timeline.init False
                    |> Timeline.update (Time.millisToPosix 0)
                    |> interruptAt 500
                    |> render loop
                    |> startsAt 0.5
        ]


initial : Timeline.Timeline Bool
initial =
    Timeline.init False
        |> Timeline.update (Time.millisToPosix 10037)


interruptAt : Int -> Timeline.Timeline Bool -> Timeline.Timeline Bool
interruptAt time timeline =
    timeline
        |> Timeline.update (Time.millisToPosix time)
        |> Timeline.to (Anim.ms 1000) True
        |> Timeline.update (Time.millisToPosix time)


steps : List Anim.Step
steps =
    [ Anim.set [ Anim.opacity 0 ]
    , Anim.step (Anim.ms 1000) [ Anim.opacity 1 |> Anim.withTransition Transition.linear ]
    ]


loop : List Anim.Step
loop =
    [ Anim.loop steps ]


render : List Anim.Step -> Timeline.Timeline Bool -> Anim.Css
render resting timeline =
    Anim.onTimelineWith timeline
        (\state ->
            if state then
                ( [ Anim.opacity 0.25 |> Anim.withTransition Transition.linear ], [] )

            else
                ( [ Anim.opacity 0 ], resting )
        )
        |> Anim.toCss


startsAt : Float -> Anim.Css -> Expect.Expectation
startsAt expected css =
    case Css.keyframes css.keyframes of
        Ok blocks ->
            case
                blocks
                    |> List.reverse
                    |> List.head
                    |> Maybe.andThen (.frames >> List.head)
                    |> Maybe.andThen (\frame -> Dict.get "opacity" frame.declarations)
                    |> Maybe.andThen String.toFloat
            of
                Just actual ->
                    Expect.within (Expect.Absolute 0.001) expected actual

                Nothing ->
                    Expect.fail "Expected an opacity keyframe for the departing transition"

        Err error ->
            Expect.fail error

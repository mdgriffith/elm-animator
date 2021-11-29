module Animations exposing (suite)

{-| For testing end-to-end animation
-}

import Animator
import Animator.Timeline
import Expect
import Fuzz exposing (Fuzzer, float, int, list, string)
import Internal.Time as Time
import Internal.Timeline
import Test exposing (..)
import Time


suite =
    -- only <|
    describe "Animation"
        [ test "A simple animation to opacity generates a transition" <|
            \_ ->
                let
                    css =
                        Animator.css
                            (Animator.Timeline.init []
                                |> Animator.Timeline.to (Animator.ms 1000)
                                    [ Animator.opacity 0.5
                                    ]
                                |> Animator.Timeline.update (Time.millisToPosix 1)
                            )
                            (\animated ->
                                ( animated, [] )
                            )
                in
                Expect.true
                    "Expected a `transition` to be in the props list"
                    (List.any
                        (\( k, v ) ->
                            k == "transition"
                        )
                        css.props
                    )
        , test "A simple animation to opacity generates a transition, even after an update" <|
            \_ ->
                let
                    css =
                        Debug.log "Simple2" <|
                            Animator.css
                                (Animator.Timeline.init []
                                    |> Animator.Timeline.to (Animator.ms 1000)
                                        [ Animator.opacity 0.5
                                        ]
                                    |> Animator.Timeline.update (Time.millisToPosix 1)
                                    |> Animator.Timeline.update (Time.millisToPosix 2)
                                )
                                (\animated ->
                                    ( animated, [] )
                                )
                in
                Expect.true
                    "Expected a `transition` to be in the props list"
                    (List.any
                        (\( k, v ) ->
                            k == "transition"
                        )
                        css.props
                    )
        , test "timeline used for Animator.transition generates a `transition`" <|
            \_ ->
                let
                    transitionDuration =
                        Animator.ms 1000

                    imminent =
                        Time.absolute (Time.millisToPosix 1)

                    startTime =
                        Time.advanceBy transitionDuration imminent

                    timeline =
                        Internal.Timeline.Timeline
                            { initial = []
                            , now = imminent
                            , delay = Time.zeroDuration
                            , scale = 1
                            , events =
                                Internal.Timeline.Timetable
                                    [ Internal.Timeline.Line
                                        imminent
                                        (Internal.Timeline.Occurring
                                            [ Animator.opacity 0.5
                                            ]
                                            startTime
                                            startTime
                                        )
                                        []
                                    ]
                            , queued = Nothing
                            , interruption = []
                            , running = True
                            }

                    css =
                        -- Debug.log "Manual" <|
                        Animator.css timeline
                            (\animated -> ( animated, [] ))
                in
                Expect.true
                    "Expected a `transition` to be in the props list and no keygrames present"
                    (List.any
                        (\( k, v ) ->
                            k == "transition"
                        )
                        css.props
                        && String.isEmpty css.keyframes
                    )
        ]

module Animator.Watcher exposing
    ( Animator
    , init, watching, watchingWith, list
    , update, toSubscription
    )

{-|

@docs Animator

@docs init, watching, watchingWith, list

@docs update, toSubscription

-}

import Animator.Timeline exposing (Timeline)
import Browser.Events
import Internal.Time as Time
import Internal.Timeline as Timeline
import Time


{-| An `Animator` knows how to read and write all the `Timelines` within your `Model`.

Here's an animator from the [Checkbox.elm example](https://github.com/mdgriffith/elm-animator/blob/master/examples/Checkbox.elm),

    animator : Animator.Animator Model
    animator =
        Animator.init
            |> Animator.watching
                -- we tell the animator how
                -- to get the checked timeline using .checked
                .checked
                -- and we tell the animator how
                -- to update that timeline as well
                (\newChecked model ->
                    { model | checked = newChecked }
                )

Notice you could add any number of timelines to this animator:

    animator : Animator.Animator Model
    animator =
        Animator.init
            |> Animator.watching
                -- we tell the animator how
                -- to get the checked timeline using .checked
                .checked
                -- and we tell the animator how
                -- to update that timeline as well
                (\newChecked model ->
                    { model | checked = newChecked }
                )
            |> Animator.watching
                .anotherChecked
                (\anotherCheckboxState ->
                    { model | anotherChecked = anotherCheckboxState }
                )

**Note** — You likely only need one animator for a given project.

**Note 2** — Once we have an `Animator Model`, we have two more steps in order to set things up:

  - [create a _subscription_](#toSubscription)
  - [_update_ our model](#update)

-}
type alias Animator model =
    Timeline.Animator model


{-| -}
init : Animator model
init =
    Timeline.Animator (\_ -> { running = False, ping = Nothing }) (\now model -> model)


{-| `watching` will ensure that [`AnimationFrame`](https://package.elm-lang.org/packages/elm/browser/latest/Browser-Events#onAnimationFrame) is running when the animator is transformed into a [`subscription`](#toSubscription).

**Note** — It will actually make the animation frame subscription run all the time! At some point you'll probably want to optimize when the subscription runs, which means either using [`watchingWith`](#watchingWith) or `Animator.Css.watching`.

-}
watching :
    (model -> Timeline state)
    -> (Timeline state -> model -> model)
    -> Animator model
    -> Animator model
watching get set (Timeline.Animator getDetails updateModel) =
    Timeline.Animator
        -- always runs
        (\model ->
            { running = True
            , ping = getDetails model |> .ping
            }
        )
        (\now model ->
            let
                newModel =
                    updateModel now model
            in
            set (Timeline.update now (get newModel)) newModel
        )


{-| -}
list :
    (model -> List (Timeline state))
    -> (List (Timeline state) -> model -> model)
    -> Animator model
    -> Animator model
list getItemTimelines setItemTimelines (Timeline.Animator getDetails updateModel) =
    Timeline.Animator
        -- always runs
        (\model ->
            { running = True
            , ping = getDetails model |> .ping
            }
        )
        (\now model ->
            let
                newModel =
                    updateModel now model

                timelines =
                    getItemTimelines newModel
            in
            setItemTimelines
                (List.map (Timeline.update now)
                    timelines
                )
                newModel
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
            let
                prev =
                    isRunning model

                timeline =
                    get model

                ping =
                    case Timeline.sendPing timeline of
                        Nothing ->
                            prev.ping

                        Just currentPing ->
                            case prev.ping of
                                Nothing ->
                                    Just currentPing

                                Just prevPing ->
                                    if prevPing.delay < currentPing.delay then
                                        Just prevPing

                                    else
                                        Just currentPing
            in
            { running =
                -- if we're already running, skip
                if prev.running then
                    prev.running

                else if Timeline.needsUpdate timeline then
                    True

                else
                    eventIsRestable (Timeline.current timeline)
            , ping =
                ping
            }
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
toSubscription toMsg model (Timeline.Animator getContext _) =
    let
        context =
            getContext model
    in
    if context.running || not (context.ping == Nothing) then
        Sub.batch
            [ if context.running then
                Browser.Events.onAnimationFrame
                    toMsg

              else
                Sub.none
            , case context.ping of
                Just ping ->
                    Time.every ping.delay
                        (\time ->
                            toMsg ping.target
                        )

                Nothing ->
                    Sub.none
            ]

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

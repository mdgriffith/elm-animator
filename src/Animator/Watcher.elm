module Animator.Watcher exposing
    ( Watching
    , init, watching, list
    , update, toSubscription
    )

{-|

@docs Watching

@docs init, watching, list

@docs update, toSubscription

-}

import Animator.Timeline exposing (Timeline)
import Browser.Events
import Internal.Time as Time
import Internal.Timeline as Timeline
import Time


{-| An `Animator` knows how to read and write all the `Timelines` within your `Model`.

Here's an animator from the [Checkbox.elm example](https://github.com/mdgriffith/elm-animator/blob/master/examples/Checkbox.elm),

    import Animator.Watcher as Watcher

    animator : Watcher.Watching Model
    animator =
        Watcher.init
            |> Watcher.watching
                -- we tell the animator how
                -- to get the checked timeline using .checked
                .checked
                -- and we tell the animator how
                -- to update that timeline as well
                (\newChecked model ->
                    { model | checked = newChecked }
                )

Notice you could add any number of timelines to this animator:

    animator : Watcher.Watching Model
    animator =
        Watcher.init
            |> Watcher.watching
                .checked
                (\newChecked model ->
                    { model | checked = newChecked }
                )
            |> Watcher.watching
                .anotherChecked
                (\anotherCheckboxState ->
                    { model | anotherChecked = anotherCheckboxState }
                )

**Note** — You likely only need one animator for a given project.

**Note 2** — Once we have an `Animator Model`, we have two more steps in order to set things up:

  - [create a _subscription_](#toSubscription)
  - [_update_ our model](#update)

-}
type alias Watching model =
    Timeline.Animator model


{-| -}
init : Watching model
init =
    Timeline.Animator (\_ -> { running = False, ping = Nothing }) (\now model -> model)


{-| -}
watching :
    (model -> Timeline state)
    -> (Timeline state -> model -> model)
    -> Watching model
    -> Watching model
watching get setValue (Timeline.Animator isRunning updateModel) =
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

                running =
                    if prev.running then
                        True

                    else
                        Timeline.hasChanged timeline
                            || Timeline.justInitialized timeline
            in
            { running = running
            , ping = ping
            }
        )
        (\now model ->
            let
                newModel =
                    updateModel now model
            in
            setValue (Timeline.update now (get newModel)) newModel
        )


list :
    (model -> List item)
    -> (List item -> model -> model)
    -> Watching item
    -> Watching model
    -> Watching model
list getItems setItems (Timeline.Animator getItemRunning updateItem) (Timeline.Animator getModelRunning updateModel) =
    Timeline.Animator
        (\model ->
            let
                modelRunning =
                    getModelRunning model
            in
            List.foldl
                (\item running ->
                    Timeline.combineRunning
                        (getItemRunning item)
                        running
                )
                modelRunning
                (getItems model)
        )
        (\now model ->
            let
                newModel =
                    updateModel now model
            in
            setItems
                (List.map
                    (updateItem now)
                    (getItems newModel)
                )
                newModel
        )


{-| Convert an `Animator` to a subscription.

This is where the animator will decide if a running animation needs another frame or not.

    subscriptions model =
        Animator.toSubscription Tick model animator

-}
toSubscription : (Time.Posix -> msg) -> model -> Watching model -> Sub msg
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
update : Time.Posix -> Watching model -> model -> model
update newTime (Timeline.Animator _ updateModel) model =
    updateModel newTime model

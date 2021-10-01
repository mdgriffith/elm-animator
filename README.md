# Elm Animator

Bring animations to your Elm app!

Check out the talk that goes with the library, [The Immutable Animator's Toolkit](https://www.youtube.com/watch?v=Nf4rElfA8SE).

Join the `#animations` channel on the [Elm Slack](https://elmlang.herokuapp.com/) if you want to chat!

```elm

     import Animator as Anim
     import Html


     Anim.div
        (Anim.transition (Anim.ms 200)
            [ Anim.opacity <|
                if model.visible then
                    1
                else
                    0
            ]
        )
        []
        [ text "👍" ]
```

# The Goal of a Continuous UI

Animation can either be a nuanced, playful helper in your UI, or it can be a nuisance.

There's an immediate question of _what kinds of animation we want to cultivate in the Elm community._

With that in mind, here are two types of animation that I'd love to emphasize and `elm-animator` should be very useful for both.

## Continuous UI

_A continuous UI minimizes context-shifting for a user and allows them to build a spatial model of your interface_.

We're generally used to pieces of UI popping in and out of existence, and pages that change drastically on click. Everytime something changes, it's a context shift for your user.

We can use animation to make that experience continuous so our users can build a quick intuition about how our interfaces work.

[Sarah Drasner has an excellent talk showing what a continuous UI interaction can look like](https://youtu.be/QlmaI7x7SYo?t=167).

There's also a fairly basic [page transition example](http://mdgriffith.github.io/elm-animator/page-transition.html) - ([Code](https://github.com/mdgriffith/elm-animator/blob/master/examples/Pages.elm)) for this library that has the same principle.

## Calm Enrichment

The second type is something I think of as _calm enrichment_.

The floating city on the [Elm Japan Website](https://elmjapan.org/) —you will need to scroll down a little— is wonderful!

It doesn't distract from what I'm trying to accomplish on the website, it simply sits there, calmly floating. It doesn't take anything away from my _budget of attention_ that I have when visiting the website. In fact it likely **increases** the amount of attention I'm willing to _pay_.

They used [`elm-playground`](https://package.elm-lang.org/packages/evancz/elm-playground/latest/Playground) —which I am likewise inspired by— to achieve the effect and convinced me to make this sort of thing easy in `elm-animator`.

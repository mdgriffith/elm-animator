# Elm Animator

Bring animations to your Elm app!

Check out the talk that goes with the library, [The Immutable Animator's Toolkit](https://www.youtube.com/watch?v=Nf4rElfA8SE).

Join the `#animations` channel on the [Elm Slack](https://elmlang.herokuapp.com/) if you want to chat!

# Animating with Timelines

The high level idea is to track timelines of values in your model and animate between them.

So, if you were previously storing a `Bool`, now you'd store `Animator.Timeline Bool`, and you'd be able to animate the state transitions in your `view`

```elm
    div
        [ Animator.Inline.opacity model.checked <|
            \checked ->
                if checked then
                   Animator.at 1

                else
                   Animator.at 0
        ]
        [ text "👍" ]
```

This library handles the animation for you!  (There is slightly more work to do to get started so start with the [**Checkbox example**](https://github.com/mdgriffith/elm-animator/blob/master/examples/Checkbox.elm))

This approach is pretty cool!  It allows you to:

- Model your animations in terms of the **`Model` you're already storing**
- Animate **any number of elements** while only tracking one timeline.  This is fantastic for rich animations that need to coordinate several components.

This library also has the ability to dynamically generate **CSS `@keyframes`**, which means your animations can be very performant.

Let's check out some examples!

# Overview of examples

**Note** if you clone this library to play with the code locally, make sure to `cd examples` and run `elm make` from there!

1. [**Checkbox**](https://github.com/mdgriffith/elm-animator/blob/master/examples/Checkbox.elm) ([live example](http://mdgriffith.github.io/elm-animator/checkbox.html)) — Animate a checkbox as it's checked.  It covers:
     
     - *How to **get started** with `elm-animator`*
     - *An idea of how to **incrementally upgrade** existing code to use animations*

2. [**Page transitions**](https://github.com/mdgriffith/elm-animator/blob/master/examples/Pages.elm) ([live example](http://mdgriffith.github.io/elm-animator/page-transition.html)) — Transition between pages 3d page transition with routing.

     - *Set up routing so there are no page **reloads*** (actually this is standard for SPAs in general).
     - *Animate the transition between pages*
     - How to do _**CSS Keyframe** generation_
  
3. [**Loading**](https://github.com/mdgriffith/elm-animator/blob/master/examples/Loading.elm) ([live example](http://mdgriffith.github.io/elm-animator/loading.html)) —Animate the loading state of a piece of content.
     
     - *How to animate **resting** states such as a loading spinner*
     - _Animate with content that's already been **deleted**_ (whaaaa?!)

4. [**Mario**](https://github.com/mdgriffith/elm-animator/blob/master/examples/Mario.elm) ([live example](http://mdgriffith.github.io/elm-animator/mario.html))— The classic Mario example!  Run around and jump.

     - *How to get started with **sprite animation***
     - *How to interact with a separate animation system such as physics code!*


# The Goal of a Continuous UI

Animation can either be a nuanced, playful helper in your UI, or it can be a nuisance.

There's an immediate question of *what kinds of animation we want to cultivate in the Elm community.*

With that in mind, here are two types of animation that I'd love to emphasize and `elm-animator` should be very useful for both.


## Continuous UI

*A continuous UI minimizes context-shifting for a user and allows them to build a spatial model of your interface*.  

We're generally used to pieces of UI popping in and out of existence, and pages that change drastically on click.  Everytime something changes, it's a context shift for your user.

We can use animation to make that experience continuous so our users can build a quick intuition about how our interfaces work.

[Sarah Drasner has an excellent talk showing what a continuous UI interaction can look like](https://youtu.be/QlmaI7x7SYo?t=167).

There's also a fairly basic [page transition example](http://mdgriffith.github.io/elm-animator/page-transition.html)  - ([Code](https://github.com/mdgriffith/elm-animator/blob/master/examples/Pages.elm)) for this library that has the same principle.



## Calm Enrichment

The second type is something I think of as *calm enrichment*.

The floating city on the [Elm Japan Website](https://elmjapan.org/) —you will need to scroll down a little— is wonderful!

It doesn't distract from what I'm trying to accomplish on the website, it simply sits there, calmly floating.  It doesn't take anything away from my *budget of attention* that I have when visiting the website.  In fact it likely **increases** the amount of attention I'm willing to *pay*.

They used [`elm-playground`](https://package.elm-lang.org/packages/evancz/elm-playground/latest/Playground) —which I am likewise inspired by— to achieve the effect and convinced me to make this sort of thing easy in `elm-animator`.


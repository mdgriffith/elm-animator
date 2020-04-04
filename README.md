# Elm Animator

Bring animations to your Elm app!

Check out the talk that goes with the library, [The Immutable Animator's Toolkit](https://www.youtube.com/watch?v=Nf4rElfA8SE)!

The high level idea is to track timelines of values in your model and animate between them.

So, if you were previously storing a `Bool`, now you'd store `Animator.Timeline Bool`, and you'd be able to animate the state transitions in your `view` 

    div
        [ Animator.Inline.opacity model.checked <|
            \checked ->
                if checked then
                    1

                else
                    0
        ]
        [ text "👍" ]

This library handles the animation for you!  (There is slightly more work to do to get started so start with the [**Checkbox example**](https://github.com/mdgriffith/elm-animator/blob/master/examples/Checkbox.elm))

This approach is pretty cool!  It allows you to:

- Model your animations in terms of the **`Model` you're already storing**
- Animate **any number of elements** while only tracking one timeline.  This is fantastic for rich animations that need to coordinate several components.

This library also has the ability to dynamically generate **CSS `@keyframes`**, which means your animations can be very performant.

Let's check out some examples!

# Overview of examples

**Note** if you clone this library to play with the code locally, make sure to `cd examples` and run `elm make` from there!

1. [**Checkbox**](https://github.com/mdgriffith/elm-animator/blob/master/examples/Checkbox.elm) - Animate a checkbox as it's checked.  It covers:
     - *How to **get started** with `elm-animator`*
     - *An idea of how to **incrementally upgrade** existing code to use animations*

2. [**Page transitions**](https://github.com/mdgriffith/elm-animator/blob/master/examples/Pages.elm) - Transition between pages 3d page transition with routing.
     - *Set up routing so there are no page **reloads*** (actually this is standard for SPAs in general).
     - *Animate the transition between pages*
  
3. [**Loading**](https://github.com/mdgriffith/elm-animator/blob/master/examples/Loading.elm) - Animate the loading state of a piece of content.
     - *How to animate **resting** states such as a loading spinner*
     - _Animate with content that's already been **deleted**_ (whaaaa?!)
     - How to do _**CSS Keyframe** generation_

4. [**Todo list**](https://github.com/mdgriffith/elm-animator/blob/master/examples/Todo.elm) - **Still under construction!** - An animated todo list, where items are sorted by if they're done.
     - *Ask for bounding boxes for elements*
     - *Animate something based on it's bounding box*

5. [**Mario**](https://github.com/mdgriffith/elm-animator/blob/master/examples/Mario.elm) - The classic Mario example!  Run around and jump.
     - *How to get started with **sprite animation***
     - *How to interact with a separate animation system such as physics code!*


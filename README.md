# Elm Animator

Add animations to your Elm app!

Check out the talk that goes with the library, [The Immutable Animator's Toolkit](https://www.youtube.com/watch?v=Nf4rElfA8SE)!

This library is about tracking timelines of values in your model and animating between them.

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

This library handles the animating for you!  (There is slightly more work to do to get started so start with the [**Checkbox example**]()

This approach is pretty cool!  It allows you to

- Model your animations in terms of the **model you're already storing**
- Animate **any number of elements** while only tracking one timeline.  This is fantastic for rich animations that need to coordinate several components.

This library also has the ability to dynamically generate **CSS `@keyframes`**, which means your animations can be very performant.

Let's check out some examples!


# Overview of examples

1. [**Checkbox**]() - Animate a checkbox as it's checked.  It covers:
  - *How to **get started** with `elm-animator`*
  - *An idea of how to **incrementally upgrade** existing code to use animations*
  
2. [**Loading**]() - Animate the loading state of a piece of content.  You can initially "load" something, and then update it as well.
  - *How to animate **resting** states such as a loading spinner*
  - *Animate with content that's already been **deleted*** (whaaaa?!)

3. [**Page transitions**]() - Transition between pages 3d page transition with routing.
  - *Set up routing so there are no page **reloads***
  - *Animate the transition between pages*
  - *How to capture `focus` and `hover` state in your model so you can animate it*
  
4. [**Todo list**]() - An animated todo list, where items are sorted by if they're done.
  - *Ask for bounding boxes for elements*
  - *Animate something based on it's bounding box*
  
5. [**Mario**]() - The classic Mario example!  Run around and jump.
  - *How to get started with **sprite animation***
  - *How to interact with a separate animation system such as physics code!*

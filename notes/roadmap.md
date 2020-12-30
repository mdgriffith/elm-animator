# Treasure map

Generally I'm a little wary of posting a direct roadmap because priorities can change as I learn more about how people are using this tool.

So think of this more like a treasure map! We have some idea where there might be treasure. They may or maynot pan out.  This list is less of a "this is all definitely happening" and more of a "here are the areas that I'm aware of that could be cool and warrant exploration".

With that in mind, I have a few ideas I'm excited about!


- **Debugger** - Because of the `Animator`, we have a way to peek into your existing timelines and potentially easily wire up an animation debugger.
  - See all timelines and what they're currently doing.
  - Dynamically slow down or speed up a timeline to see exactly what's happening in the animation.
  - Interactive curve editor to craft how a transition should work.

- **Direct Svg support** - You can already animate `Svg` with `Animator` by animating a `Float` value via `Animator.move` and then formatting it into the proper attribute.  However there are a few opportunities for improvement!
  - [**SMIL**](https://developer.mozilla.org/en-US/docs/Web/SVG/SVG_animation_with_SMIL) is an animation framework built into Svg.  It was deprecated by chrome, then they brought it back.  `elm-animator` could generate `SMIL` nodes just ike it generates css `@keyframes`.
    - **?** - is `SMIL` performant?  If so, is there some browser it's not performant on?

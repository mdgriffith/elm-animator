# Changelog

## 2.0.0

V2 separates CSS animations, timeline scheduling, and numeric interpolation into
four modules: `Animator`, `Animator.Timeline`, `Animator.Value`, and
`Animator.Transition`.

### New animation API

- Use `Animator.transition` for changes to CSS properties, or `Animator.keyframes`
  for explicit sequences and loops. Neither requires an Elm clock subscription.
- Use `Animator.onTimeline` to coordinate elements around application states.
  `onTimelineWith` adds resting animations, such as a loop while loading.
- Choose Bézier or spring motion with `Animator.Transition` and apply it using
  `withTransition`. Springs expose `wobble` and `quickness`, both in `0..1`.
- Built-in spinning, pulsing, bouncing, and pinging animations are available from
  `Animator`, with `toCss` for integration with other view libraries.
- Translation and scale axes can be animated independently. Timeline and
  keyframe animations support different curves on individual axes.

### Upgrading from 1.x

This is a breaking release. `Animator.Css` and `Animator.Inline` have been removed;
CSS animation now lives in `Animator`, and numeric interpolation in `Animator.Value`.

| 1.x | 2.0.0 |
| --- | --- |
| `Animator.init`, `go`, `event` | `Timeline.init`, `to`, `transitionTo` |
| `Animator.queue`, `interrupt`, `wait` | `Timeline.queue`, `interrupt`, `wait` |
| `Animator.current`, `previous`, `arrived`, `arrivedAt`, `upcoming` | Corresponding functions in `Animator.Timeline` |
| `Animator.updateTimeline` | `Timeline.update` |
| `Animator.millis n`, `seconds n` | `Animator.ms n`, `Animator.ms (n * 1000)` |
| `Animator.move`, `at` | `Value.float`, `Value.to` |
| `Animator.xy`, `xyz`, `color` | `Value.xy`, `xyz`, `color` |
| `Animator.withWobble amount` | `Value.withTransition (Transition.spring { wobble = amount, quickness = 0.5 })` as a starting point for retuning |

Here, `Timeline`, `Value`, and `Transition` refer to the corresponding
`Animator.*` modules.

- Replace the `Animator.animator` / `watching` / `toSubscription` setup with
  `Browser.Events.onAnimationFrame` while `Timeline.isRunning`, and call
  `Timeline.update` on each tick. The Timeline module includes a setup example.
- The named duration presets, sprite-frame helpers, oscillator helpers, and
  `arrive*` / `leave*` curve modifiers are no longer exposed. Use explicit
  durations, transition curves, and CSS sequences where appropriate.
- `Animator.step`, `loop`, `wait`, and `color` now describe CSS animations;
  their signatures and meanings differ from 1.x.
- The library now depends on `mdgriffith/elm-bezier` 2.x and no longer requires
  `elm-units`. Construct animation durations with `Animator.ms`.

### Behavior and fixes

- Native spring transitions use CSS `linear(...)` easing, requiring a browser
  that supports it. They follow CSS transition reversal rules; use timeline
  springs for velocity-preserving interruptions.
- Interrupted animations continue from their sampled positions, including
  departures from an initial resting loop and departures after queued waits.
- Corrected timeline traversal, arrival notifications, progress, and independent
  numeric coordinate interpolation.
- Timeline history collection preserves the five-second delay window and the
  previous reached state. Delays accumulate and are capped at five seconds.
- Sequence wrappers and directly nested repeats keep compact CSS iteration counts.
- Added automated review, Elm regression/property tests, and browser tests across
  Chromium, Firefox, and WebKit. Development tooling uses Bun with a frozen lockfile.


## 1.1.0

Added more ways to ask the `Timeline` what's happening including:

- `upcoming` - check if an event is in the future of the timeline.
- `arrived` - Similar to `current`, but only return a new state when you've fully transitioned to it instead of when the transition begins.
- `arrivedAt` - check if you will arrive at an event in the current tick.
- `updateTimeline` - manually update a timeline which is very useful for games.


## 1.0.2

A number of bug fixes around `Animator.queue`

# Changelog

## Unreleased — V2

- Consolidated CSS rendering and expanded Elm/browser regression coverage.
- Native spring transitions now use CSS `linear(...)` easing through the existing
  per-attribute API, including transitions back to default values and retargeting.
- Timeline collection preserves the supported five-second delay window and the
  previous reached state.
- Sequence wrappers and directly nested repeats retain compact CSS iteration
  counts instead of expanding every repetition into keyframes.
- Added CI for Elm tests/examples and Chromium, Firefox, and WebKit browser tests.



## 1.1.0

Added more ways to ask the `Timeline` what's happening including:

- `upcoming` - check if an event is in the future of the timeline.
- `arrived` - Similar to `current`, but only return a new state when you've fully transitioned to it instead of when the transition begins.
- `arrivedAt` - check if you will arrive at an event in the current tick.
- `updateTimeline` - manually update a timeline which is very useful for games.


## 1.0.2

A number of bug fixes around `Animator.queue`

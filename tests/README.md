# Testing V2

The Elm and browser suites pass. Previously failing V2 scenarios remain as
ordinary regression assertions, including timeline traversal, interruptions,
and garbage collection; no expected failures or skipped regressions are needed.

## Run

From the repository root:

```sh
# Install the pinned tools using Bun 1.2.3 and the committed lockfile.
bun install --frozen-lockfile

# Static review of the package source and README.
bun run review

# Elm unit, regression, and property tests.
bun run test --seed 12345 --fuzz 1000

# One-time browser installation, then the browser contract tests.
bunx --no-install playwright install chromium firefox webkit
bun run test:browser

# Both suites in one command (after installing browsers).
bun run test:all
```

Use `bun run test` to invoke elm-test; `bun test` invokes Bun's own test runner.
Node.js 22 is used in CI for the Elm and Playwright command-line tools.

On Linux, Playwright may require `bunx --no-install playwright install --with-deps chromium firefox webkit`.
The browser command compiles an optimized Elm fixture against this checkout's
`src/`, not the published package. Generated files live in ignored `elm-stuff/`
and `test-results/` directories. No development server is needed.

To focus on one area:

```sh
bun run test tests/Rendering.elm --seed 12345
bun run test tests/Values.elm tests/TimelineLaws.elm --seed 12345 --fuzz 1000
bun run test tests/Scheduling.elm --seed 12345 --fuzz 1000
bun run test tests/TimelineRetention.elm --seed 67890 --fuzz 10000
bun run test:browser --grep "queued transition"
bun run test:browser --project=webkit
```

Elm prints the seed and shrunk input for a failing fuzz test. Keep useful shrunk
inputs as ordinary regression tests as well; `Scheduling.elm` includes one found
while increasing the fuzz count in this review.

### Recorded baseline

After the history-retention, native spring, repeat-compaction, and initial
resting-animation continuity changes:

| Command | Passed | Failed |
| --- | ---: | ---: |
| `bun run test --seed 67890 --fuzz 10000` | 264 | 0 |
| `bun run test:browser` | 90 | 0 |

Browser results cover 30 scenarios in each of Chromium, Firefox, and WebKit.
GC invariance is exercised by fuzz tests and saved minimal examples, including
inspectors and movement throughout the supported five-second delay window.
All test modules and the browser fixture compile.

## Continuous integration

`.github/workflows/test.yml` runs on pushes and pull requests. One job runs
`elm-review`, the Elm suite with 10,000 fuzz iterations, and compiles all examples. Separate browser
jobs install and test Chromium, Firefox, and WebKit. Failed browser jobs upload
Playwright traces and screenshots. Tool versions are pinned by `package.json`
and `bun.lock`. CI installs with `bun install --frozen-lockfile`.

## Coverage

| Suite | Contract |
| --- | --- |
| `Rendering.elm` | Same initial-value, grouping, property-presence, easing, and animation-identity expectations for `onTimeline`, `onTimelineWith` with no steps, and `css` with no steps |
| `UnifiedRendering.elm` | Native Bézier/spring transitions, default-value targets, zero-duration steps, compact nested repeats, fractional durations, animation identity, sampled interruption positions, and a 10,000-event rendering stress test |
| `InitialResting.elm` | Initial loop interruption positions, repeat phase, finite completion, queued waits, stable idle CSS, and first-tick clock anchoring |
| `SpringIntegration.elm` | Segment time domains independent of target position, initial velocity, consistent explicit intro velocity, and momentum when retargeting to the current position |
| `Values.elm` | Independent coordinates, color endpoints, zero-duration motion, linear position/velocity, interruption position, and completed motion |
| `TimelineLaws.elm` | Scheduling flags, completion, latest interruption, waits, progress, scaling, delay, clock-update invariance, and a small independent reference model for queued movement |
| `TimelineRetention.elm` | Previous-state retention across completed lines, delayed sampling after collection, and collected/uncollected equivalence for all inspectors and motion within the delay window |
| `Animations.elm` | Standalone transitions versus timeline keyframes; stable CSS across ordinary ticks |
| `Keyframes.elm` | Endpoints, initial easing, spring bounds/overshoot, and empty intervals |
| `Sequencing.elm` | Full, partially consumed, delayed, and replacement movement sequences |
| Existing `Scheduling`, `TimelineState`, `Randomness` suites | Ordering, collection, state inspectors, large schedules, and randomness |
| `../browser-tests/` | Actual computed styles, native spring return/retargeting, mixed axis curves, colors, queued motion, waits, compact nested repeats, interruptions, delayed collection, and animation identity after an unrelated Elm update |

The CSS inspection helper reads the generated keyframe subset without comparing
whole stylesheets, whitespace, or animation hashes. It is not a general CSS
parser; browser tests establish that CSS accepted by the browser behaves as
intended. Numeric interpolation tests allow for approximate Bézier inversion.

Browser motion is sampled by pausing animations and setting their `currentTime`
through the Web Animations API. No sleeps or wall-clock animation progress are
used. The initial DOM style matches the fixture's starting opacity so queued
timing failures can be distinguished from implicit browser starting values.
All browser scenarios run in Chromium, Firefox, and WebKit.

## Regression coverage

| Area | Verified behavior |
| --- | --- |
| Timeline progress | Reports the active transition; future queued transitions do not overwrite progress |
| Timeline delay | Delays add together, negative additions are ignored, and the total is capped at five seconds |
| Value interpolation | XYZ channels are independent; instantaneous changes reach their targets; completed movement has zero velocity |
| Interruptions | Numeric and color motion continue from the sampled interruption point, preserving spring momentum |
| Initial resting steps | The first clock update anchors the initial resting animation; departures sample its current position, including after queued waits |
| State inspection | Arrival occurs before a dwell; canceled destinations are excluded; arrival notifications are not repeated on the next tick |
| Garbage collection | Retains the five-second lookback window and the previous reached state; collection preserves delayed inspectors, position, velocity, and CSS phase |
| Native springs | CSS `linear(...)` starts from the browser's current value, including returning to zero and retargeting an active transition |
| Repeat compilation | Run-once wrappers do not expand repetitions; directly nested repeat counts remain compact CSS iteration counts |

The former rendering regressions (initial values, grouped transforms, omitted
custom properties, invalid animation lists, and queued timing) also pass. Both
CSS and `Animator.Value` interruption behavior are tested independently.

Timeline inspection and value interpolation share a traversal that visits each
reachable transition once, with its end clipped to the next interruption.
Sampling freezes abandoned motion at that boundary. Arrivals use the target's
arrival time rather than the end of its dwell, which also makes return-duration
discounting recognize states that have already been reached.

## Rendering architecture

All public rendering APIs delegate to `InternalAnim.Render`. `onTimeline` is the
empty-steps case of `onTimelineWith`, and `css` extracts that same result.
`InternalAnim.Property` contains attribute definitions, not another renderer.
The old `InternalAnim.Css` renderer and `Move.cssForSections` serializer have
been removed.

- `Animator.transition` uses native transitions for Bézier curves and sampled
  CSS `linear(...)` spring easing. The existing per-attribute `withTransition`
  API is retained; there is no `transitionWith` or capability type parameter.
- Timeline and explicit sequence animations always use keyframes. Each animation
  has explicit endpoints and a delay relative to the schedule's rendering origin.
  Later animations take precedence only when they start.
- Translation and scale channels are grouped into their corresponding CSS
  properties. Compatible curves remain native cubic Béziers; springs and mixed
  channel curves are sampled with a bounded frame count.
- Ordinary ticks preserve animation identity. If collection actually removes
  history, the rendering origin advances once and negative delays preserve the
  current position of animations already in progress.
- Initial resting steps have a separate first-update time anchor, so scheduling
  a departure does not discard their elapsed time or restart their loop phase.
  Collection advances this anchor to the retained history boundary.
- Collection anchors at the latest reached state before the five-second
  lookback window, retaining its original arrival/dwell times and the preceding
  reached state. Unfinished interruption chains may require older history; the
  five-second window is a minimum retention guarantee, not a hard memory bound.
- Run-once sequences are flattened and directly nested repeats are combined
  before rendering. The 1,000-repeat wrapper regression now produces exactly
  the same CSS as the unwrapped repeat rather than roughly 123 KB of keyframes.
  Repeats embedded between other timed steps inside a repeatedly looping parent
  can still require expanded keyframes; these more complex shapes are not a
  constant-size representation.

### Native transition boundaries

Native spring easing requires browsers with CSS `linear()` support (widely
available across browsers since December 2023). It preserves the current
starting position on retargeting, but follows CSS transition timing/reversal
rules rather than preserving physical spring velocity. Use timelines for
velocity-aware spring interruptions.

CSS gives each compound property (`translate` or `scale`) a single timing
function. Apply the same curve to all supplied axes to keep the transition
native. Different curves on those axes still use the existing keyframe fallback;
use a timeline or explicit keyframes with starting values when independently
eased axes need interruption-safe behavior.

Animator now requires `mdgriffith/elm-bezier` 2.x. The integration tests verify
that `Spring.segments` spans the actual settling time regardless of target
position, and that `Spring.at` honors incoming velocity. Animator's wrapper
preserves that velocity at zero progress and applies an explicit nonzero intro
velocity consistently throughout the spring.

The renderer continues to sample `Transition.atX`/`Spring.at` at the requested
animation times, which need not equal the estimated settling time. This also
handles mixed curves within compound properties and lands exactly on the target
at the requested end time. The lower-level segment-to-keyframe helper maps the
actual settling time into the requested percentage interval.

## Existing-suite repairs

- Replaced references to removed/internal helpers with current APIs.
- Updated transition tests to distinguish `Animator.transition` (CSS transition)
  from timeline-controlled motion (CSS keyframes).
- Replaced brittle keyframe snapshots with endpoint/easing/domain assertions.
- Reworked sequence sampling to start replacement motion at the interruption
  position rather than replaying an untrimmed, abandoned transition.
- Replaced vacuous stress-test assertions with checks of their resulting motion
  or scheduled destinations.

During the initial suite repair, the final spring keyframe was normalized by
the same factor of 1000 as intermediate frames. The old
snapshot accepted a final value near `1005` for a movement near `0..1`; the new
test checks the value domain and approximate settling instead.

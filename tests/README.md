# Testing V2

The full Elm suite is intentionally red while V2's timeline/value behavior is being sorted out. The
regressions are ordinary assertions: they are not skipped, marked as expected
failures, or changed to accept the current incorrect output. The rendering
contracts and browser suite now pass through the unified renderer.

## Run

From the repository root:

```sh
# Install the pinned tools using the existing pnpm lockfile.
npx --yes pnpm@8.15.9 install --frozen-lockfile

# Elm unit, regression, and property tests.
npm test -- --seed 12345 --fuzz 1000

# One-time browser installation, then the browser contract tests.
npx playwright install chromium
npm run test:browser
```

On Linux, Playwright may require `npx playwright install --with-deps chromium`.
The browser command compiles an optimized Elm fixture against this checkout's
`src/`, not the published package. Generated files live in ignored `elm-stuff/`
and `test-results/` directories. No development server is needed.

To focus on one area:

```sh
npm test -- tests/Rendering.elm --seed 12345
npm test -- tests/Values.elm tests/TimelineLaws.elm --seed 12345 --fuzz 1000
npm test -- tests/Scheduling.elm --seed 12345 --fuzz 1000
npm run test:browser -- --grep "queued transition"
```

Elm prints the seed and shrunk input for a failing fuzz test. Keep useful shrunk
inputs as ordinary regression tests as well; `Scheduling.elm` includes one found
while increasing the fuzz count in this review.

### Recorded baseline

After consolidating rendering and upgrading to elm-bezier 2.0.0:

| Command | Passed | Failed |
| --- | ---: | ---: |
| `npm test -- --seed 12345 --fuzz 1000` | 225 | 7 |
| `npm run test:browser` | 24 | 0 |

The GC regression appears as both a fuzz failure and a saved example. Failure
counts are not counts of distinct bugs. All test modules and the browser fixture compile.

## Coverage

| Suite | Contract |
| --- | --- |
| `Rendering.elm` | Same initial-value, grouping, property-presence, easing, and animation-identity expectations for `onTimeline`, `onTimelineWith` with no steps, and `css` with no steps |
| `UnifiedRendering.elm` | Native transitions, spring fallback, zero-duration steps, empty/zero-count loops, fractional durations, animation identity, sampled interruption positions, and a 10,000-event rendering stress test |
| `SpringIntegration.elm` | Segment time domains independent of target position, initial velocity, consistent explicit intro velocity, and momentum when retargeting to the current position |
| `Values.elm` | Independent coordinates, color endpoints, zero-duration motion, linear position/velocity, interruption position, and completed motion |
| `TimelineLaws.elm` | Scheduling flags, completion, latest interruption, waits, progress, scaling, delay, clock-update invariance, and a small independent reference model for queued movement |
| `Animations.elm` | Standalone transitions versus timeline keyframes; stable CSS across ordinary ticks |
| `Keyframes.elm` | Endpoints, initial easing, spring bounds/overshoot, and empty intervals |
| `Sequencing.elm` | Full, partially consumed, delayed, and replacement movement sequences |
| Existing `Scheduling`, `TimelineState`, `Randomness` suites | Ordering, collection, state inspectors, large schedules, and randomness |
| `../browser-tests/` | Actual computed styles, native transitions, spring fallback, mixed axis curves, colors, queued motion, waits, nested/finite/infinite loops, interruptions, collection, and animation identity after an unrelated Elm update |

The CSS inspection helper reads the generated keyframe subset without comparing
whole stylesheets, whitespace, or animation hashes. It is not a general CSS
parser; browser tests establish that CSS accepted by the browser behaves as
intended. Numeric interpolation tests allow for approximate Bézier inversion.

Browser motion is sampled by pausing animations and setting their `currentTime`
through the Web Animations API. No sleeps or wall-clock animation progress are
used. The initial DOM style matches the fixture's starting opacity so queued
timing failures can be distinguished from implicit browser starting values.
Browser coverage currently runs in Chromium.

## Known regressions retained

These are behavioral failures, not compilation problems:

| Area | Demonstrated failure |
| --- | --- |
| Timeline progress | Progress during the first of two queued transitions describes a later transition instead of the active one |
| Timeline delay | A requested 200ms delay samples the wrong time |
| Value interpolation | `xyz.y` mirrors X; a zero-duration transition retains the old value; interrupted movement uses the abandoned trajectory rather than its sampled interruption position |
| Garbage collection | Collection can change position at an immediate interruption; covered by both fuzzing and a saved minimal reproduction |

The former rendering regressions (initial values, grouped transforms, omitted
custom properties, invalid animation lists, and queued timing) now pass. CSS
interruptions also pass; the remaining interrupted-motion failure is in
`Animator.Value`, which is tested independently.

## Rendering architecture

All public rendering APIs delegate to `InternalAnim.Render`. `onTimeline` is the
empty-steps case of `onTimelineWith`, and `css` extracts that same result.
`InternalAnim.Property` contains attribute definitions, not another renderer.
The old `InternalAnim.Css` renderer and `Move.cssForSections` serializer have
been removed.

- `Animator.transition` explicitly permits native transitions for compatible
  Bézier curves. Springs and mixed curves within one compound property use
  keyframes instead.
- Timeline and explicit sequence animations always use keyframes. Each animation
  has explicit endpoints and a delay relative to the schedule's rendering origin.
  Later animations take precedence only when they start.
- Translation and scale channels are grouped into their corresponding CSS
  properties. Compatible curves remain native cubic Béziers; springs and mixed
  channel curves are sampled with a bounded frame count.
- Ordinary ticks preserve animation identity. If collection actually removes
  history, the rendering origin advances once and negative delays preserve the
  current position of animations already in progress.

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

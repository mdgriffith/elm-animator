const { test, expect } = require('@playwright/test');
const path = require('node:path');

async function mount(page, renderer, scenario) {
  // Match the underlying style to the declared starting value, so sequencing
  // failures are not merely consequences of an implicit browser start value.
  const initialOpacity = scenario === 'queued' ? 1 : 0;
  await page.setContent(`<style>#subject { opacity: ${initialOpacity}; }</style><div id="app"></div>`);
  await page.addScriptTag({ path: path.join(__dirname, '../elm-stuff/browser-tests.js') });
  await page.evaluate(({ renderer, scenario }) => {
    Elm.Main.init({ node: document.getElementById('app'), flags: { renderer, scenario } });
  }, { renderer, scenario });
  await expect(page.locator('#subject')).toBeAttached();
}

// Seek the browser's actual CSS animations instead of sleeping until a frame
// happens to arrive. Delays remain part of each animation's local timeline.
async function sampleOpacity(page, milliseconds) {
  return page.locator('#subject').evaluate((element, time) => {
    const animations = element.getAnimations();
    if (!animations.length) throw new Error('Expected a CSS animation');
    for (const animation of animations) {
      animation.pause();
      animation.currentTime = time;
    }
    return Number(getComputedStyle(element).opacity);
  }, milliseconds);
}

for (const renderer of ['onTimeline', 'onTimelineWith']) {
  test.describe(renderer, () => {
    test('preserves initial opacity', async ({ page }) => {
      await mount(page, renderer, 'initial');
      await expect(page.locator('#subject')).toHaveCSS('opacity', '0.35');
    });

    test('composes translation and scale', async ({ page }) => {
      await mount(page, renderer, 'initial');
      const computed = await page.locator('#subject').evaluate(element => {
        const style = getComputedStyle(element);
        return { translate: style.translate, scale: style.scale };
      });
      expect(computed).toEqual({ translate: '12px 34px 56px', scale: '2 2 2' });
    });

    test('preserves initial color', async ({ page }) => {
      await mount(page, renderer, 'initial');
      await expect(page.locator('#subject')).toHaveCSS('background-color', 'rgb(200, 50, 25)');
    });

    test('interpolates a single linear transition in the browser', async ({ page }) => {
      await mount(page, renderer, 'single');
      expect.soft(await sampleOpacity(page, 250), 'at 250ms').toBeCloseTo(0.25, 2);
      expect.soft(await sampleOpacity(page, 500), 'at 500ms').toBeCloseTo(0.5, 2);
      expect.soft(await sampleOpacity(page, 1000), 'at 1000ms').toBeCloseTo(1, 2);
    });

    test('honors queued transition and wait durations', async ({ page }) => {
      await mount(page, renderer, 'queued');
      expect.soft(await sampleOpacity(page, 500), 'during first transition').toBeCloseTo(0.75, 2);
      expect.soft(await sampleOpacity(page, 1250), 'during wait').toBeCloseTo(0.5, 2);
      expect.soft(await sampleOpacity(page, 2000), 'during second transition').toBeCloseTo(0.25, 2);
      expect.soft(await sampleOpacity(page, 2500), 'after both transitions').toBeCloseTo(0, 2);
    });

    test('an unrelated Elm update preserves the existing animation', async ({ page }) => {
      await mount(page, renderer, 'single');
      await sampleOpacity(page, 400);
      await page.locator('#subject').evaluate(element => {
        window.originalAnimations = element.getAnimations();
      });
      await page.locator('#update').click();
      await expect(page.locator('#updates')).toHaveText('1');
      const preserved = await page.locator('#subject').evaluate(element => {
        const current = element.getAnimations();
        return current.length === window.originalAnimations.length &&
          current.every((animation, index) =>
            animation === window.originalAnimations[index] && animation.currentTime === 400);
      });
      expect(preserved).toBe(true);
    });
  });
}

test.describe('standalone keyframes', () => {
  test('directly nested repeats use a single browser iteration count', async ({ page }) => {
    await mount(page, 'onTimeline', 'nested-repeat');
    expect(await sampleOpacity(page, 500)).toBeCloseTo(0.5, 2);
    expect(await sampleOpacity(page, 5500)).toBeCloseTo(0.5, 2);
    expect(await sampleOpacity(page, 6500)).toBeCloseTo(1, 2);
    const timing = await page.locator('#subject').evaluate(element =>
      element.getAnimations().map(animation => ({
        duration: animation.effect.getTiming().duration,
        iterations: animation.effect.getTiming().iterations,
      })));
    expect(timing).toEqual([{ duration: 1000, iterations: 6 }]);
  });

  test('a finite loop repeats twice and holds its final value', async ({ page }) => {
    await mount(page, 'onTimeline', 'finite-loop');
    expect.soft(await sampleOpacity(page, 500), 'first iteration').toBeCloseTo(0.5, 2);
    expect.soft(await sampleOpacity(page, 1500), 'second iteration').toBeCloseTo(0.5, 2);
    expect.soft(await sampleOpacity(page, 2500), 'after completion').toBeCloseTo(1, 2);
    const iterations = await page.locator('#subject').evaluate(element =>
      element.getAnimations().map(animation => animation.effect.getTiming().iterations));
    expect(iterations).toEqual([2]);
  });

  test('an infinite loop continues beyond two iterations', async ({ page }) => {
    await mount(page, 'onTimeline', 'infinite-loop');
    expect.soft(await sampleOpacity(page, 2500)).toBeCloseTo(0.5, 2);
    const infinite = await page.locator('#subject').evaluate(element =>
      element.getAnimations().some(animation => animation.effect.getTiming().iterations === Infinity));
    expect(infinite).toBe(true);
  });

  test('nested finite loops restart and preserve subsequent waits', async ({ page }) => {
    await mount(page, 'onTimeline', 'nested');
    expect(await sampleOpacity(page, 500)).toBeCloseTo(0.5, 2);
    expect(await sampleOpacity(page, 1500)).toBeCloseTo(0.5, 2);
    expect(await sampleOpacity(page, 2250)).toBeCloseTo(1, 2);
    expect(await sampleOpacity(page, 2750)).toBeCloseTo(0.5, 2);
    expect(await sampleOpacity(page, 3000)).toBeCloseTo(0, 2);
  });

  test('an instantaneous set inside a loop does not erase the preceding movement', async ({ page }) => {
    await mount(page, 'onTimeline', 'instant-reset');
    expect(await sampleOpacity(page, 500)).toBeCloseTo(0.5, 2);
    expect(await sampleOpacity(page, 999)).toBeCloseTo(0.999, 2);
    expect(await sampleOpacity(page, 1000)).toBeCloseTo(0, 2);
    expect(await sampleOpacity(page, 1500)).toBeCloseTo(0.5, 2);
  });
});

async function trigger(page) {
  await page.locator('#trigger').click();
  await expect(page.locator('#updates')).toHaveText('1');
}

test.describe('unified pipeline behavior', () => {
  test('an interrupted spring carries incoming momentum even at its target position', async ({ page }) => {
    await mount(page, 'onTimeline', 'spring-interruption');
    const x = () => page.locator('#subject').evaluate(element => parseFloat(getComputedStyle(element).translate));
    await sampleOpacity(page, 500);
    expect(await x()).toBeCloseTo(100, 2);
    await trigger(page);
    await sampleOpacity(page, 0);
    expect(await x()).toBeCloseTo(100, 2);
    await sampleOpacity(page, 100);
    expect(await x()).toBeGreaterThan(101);
    await sampleOpacity(page, 1000);
    expect(await x()).toBeCloseTo(100, 2);
  });

  test('collecting completed timeline events preserves the active CSS position', async ({ page }) => {
    await mount(page, 'onTimeline', 'gc');
    expect(await sampleOpacity(page, 8250)).toBeCloseTo(0.86875, 3);
    await trigger(page);
    expect(await sampleOpacity(page, 0)).toBeCloseTo(0.86875, 3);
    expect(await sampleOpacity(page, 750)).toBeCloseTo(0.8875, 3);
  });

  test('the maximum view delay preserves CSS phase through collection', async ({ page }) => {
    await mount(page, 'onTimeline', 'gc-delayed');
    expect(await sampleOpacity(page, 8250)).toBeCloseTo(0.75, 3);
    await trigger(page);
    expect(await sampleOpacity(page, 0)).toBeCloseTo(0.75, 3);
    expect(await sampleOpacity(page, 750)).toBeCloseTo(0.7625, 3);
  });

  test('explicit transitions use actual browser CSSTransitions', async ({ page }) => {
    await mount(page, 'onTimeline', 'native');
    await expect(page.locator('#subject')).toHaveCSS('opacity', '0');
    await trigger(page);
    expect(await sampleOpacity(page, 500)).toBeCloseTo(0.5, 2);
    const native = await page.locator('#subject').evaluate(element =>
      element.getAnimations().every(animation => animation instanceof CSSTransition));
    expect(native).toBe(true);
  });

  test('spring transitions use native linear() easing, overshoot, and settle', async ({ page }) => {
    await mount(page, 'onTimeline', 'spring');
    await trigger(page);
    await sampleOpacity(page, 250);
    const overshoot = await page.locator('#subject').evaluate(element => ({
      x: parseFloat(getComputedStyle(element).translate),
      native: element.getAnimations().every(animation => animation instanceof CSSTransition),
      easing: getComputedStyle(element).transitionTimingFunction,
    }));
    expect(overshoot.native).toBe(true);
    expect(overshoot.easing).toMatch(/^linear\(/);
    expect(overshoot.x).toBeGreaterThan(100);
    expect(overshoot.x).toBeLessThan(150);
    await sampleOpacity(page, 1000);
    const final = await page.locator('#subject').evaluate(element => parseFloat(getComputedStyle(element).translate));
    expect(final).toBeCloseTo(100, 2);
  });

  test('native springs animate back to zero from the current browser value', async ({ page }) => {
    await mount(page, 'onTimeline', 'spring');
    await trigger(page);
    await sampleOpacity(page, 1000);
    await page.locator('#return').click();
    await expect(page.locator('#updates')).toHaveText('2');
    await sampleOpacity(page, 0);
    const x = () => page.locator('#subject').evaluate(element => parseFloat(getComputedStyle(element).translate));
    expect(await x()).toBeCloseTo(100, 2);
    await sampleOpacity(page, 250);
    expect(await x()).toBeLessThan(0);
    expect(await x()).toBeGreaterThan(-50);
    await sampleOpacity(page, 1000);
    expect(await x()).toBeCloseTo(0, 2);
  });

  test('retargeting a native spring starts at its current rendered position', async ({ page }) => {
    await mount(page, 'onTimeline', 'spring');
    await trigger(page);
    await sampleOpacity(page, 100);
    const x = () => page.locator('#subject').evaluate(element => parseFloat(getComputedStyle(element).translate));
    const before = await x();
    expect(before).toBeGreaterThan(0);
    await page.locator('#retarget').click();
    await expect(page.locator('#updates')).toHaveText('2');
    await sampleOpacity(page, 0);
    expect(await x()).toBeCloseTo(before, 2);
    await sampleOpacity(page, 1000);
    expect(await x()).toBeCloseTo(150, 2);
  });

  test('compound transforms preserve independent channel curves', async ({ page }) => {
    await mount(page, 'onTimeline', 'mixed-axis');
    expect(await sampleOpacity(page, 500)).toBeCloseTo(0.75, 2);
    const xyz = await page.locator('#subject').evaluate(element =>
      getComputedStyle(element).translate.split(' ').map(parseFloat));
    expect(xyz[0]).toBeCloseTo(50, 1);
    expect(xyz[1]).toBeCloseTo(12.5, 1);
    expect(xyz[2]).toBeCloseTo(30, 1);
  });

  test('colors use the requested curve and explicit endpoints', async ({ page }) => {
    await mount(page, 'onTimelineWith', 'color');
    await sampleOpacity(page, 500);
    await expect(page.locator('#subject')).toHaveCSS('background-color', 'rgb(128, 0, 128)');
  });

  test('a timeline interruption resumes at the sampled position', async ({ page }) => {
    await mount(page, 'onTimeline', 'single');
    expect(await sampleOpacity(page, 500)).toBeCloseTo(0.5, 2);
    await trigger(page);
    expect(await sampleOpacity(page, 0)).toBeCloseTo(0.5, 2);
    expect(await sampleOpacity(page, 500)).toBeCloseTo(0.625, 2);
    expect(await sampleOpacity(page, 1000)).toBeCloseTo(0.75, 2);
  });

  test('resting loops begin after arrival and can be interrupted', async ({ page }) => {
    await mount(page, 'onTimelineWith', 'resting');
    expect(await sampleOpacity(page, 500)).toBeCloseTo(0.5, 2);
    expect(await sampleOpacity(page, 1500)).toBeCloseTo(0.5, 2);
    await trigger(page);
    expect(await sampleOpacity(page, 0)).toBeCloseTo(0.5, 2);
    expect(await sampleOpacity(page, 500)).toBeCloseTo(0.625, 2);
  });
});

const { defineConfig } = require('@playwright/test');

module.exports = defineConfig({
  testDir: __dirname,
  testMatch: '*.spec.cjs',
  fullyParallel: true,
  workers: 2,
  retries: 0,
  reporter: 'list',
  use: { browserName: 'chromium', headless: true },
});

const { spawnSync } = require('node:child_process');

const result = spawnSync(
  'elm',
  ['make', 'src/Main.elm', '--optimize', '--output=../elm-stuff/browser-tests.js'],
  { cwd: __dirname, stdio: 'inherit' }
);

if (result.error) {
  console.error(result.error.message);
}
process.exit(result.status ?? 1);

// Headless smoke test: compiles sim/Sim.elm and lets bot players climb and
// fight. Prints one line per level.
// Usage: bun scripts/sim.ts [levels=1-11] [seconds=240] [players=4] [weapon=fists] [--boss]
//   --boss  skip the climb and start each level at its boss fight
const args = process.argv.slice(2);
const bossOnly = args.includes('--boss');
const [levels = '1-11', seconds = '240', players = '4', weapon = 'fists'] = args.filter((a) => !a.startsWith('--'));

const out = 'dist/sim.js';
const make = Bun.spawnSync(['elm', 'make', 'sim/Sim.elm', '--optimize', `--output=${out}`], { stdout: 'pipe', stderr: 'pipe' });
if (make.exitCode !== 0) {
  console.error(make.stdout.toString() + make.stderr.toString());
  process.exit(1);
}

interface SimApp {
  ports: { report: { subscribe(cb: (r: Record<string, unknown>) => void): void } };
}
type ElmSim = { Sim: { init(o: { flags: object }): SimApp } };
const code = await Bun.file(out).text();

const [lo = 1, hi = lo] = levels.split('-').map(Number);
for (let level = lo; level <= hi; level++) {
  const scope: { Elm?: ElmSim } = {};
  new Function(code).call(scope);
  if (!scope.Elm) throw new Error('Elm did not load');
  const t0 = performance.now();
  const app = scope.Elm.Sim.init({ flags: { level, seconds: Number(seconds), players: Number(players), weapon, bossOnly } });
  app.ports.report.subscribe((r) => {
    console.log(JSON.stringify(r), `${(performance.now() - t0).toFixed(0)}ms`);
  });
}

export {};

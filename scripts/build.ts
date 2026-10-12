// Builds the game into dist/: Elm -> dist/elm.js, TypeScript -> dist/main.js.
// Usage: bun scripts/build.ts [--optimize]
import { copyFileSync, mkdirSync } from 'node:fs';

export interface BuildResult {
  ok: boolean;
  log: string;
}

export async function build(optimize: boolean): Promise<BuildResult> {
  mkdirSync('dist', { recursive: true });
  const elmArgs = ['elm', 'make', 'src/Main.elm', '--output=dist/elm.js'];
  if (optimize) elmArgs.push('--optimize');
  const elm = Bun.spawnSync(elmArgs, { stdout: 'pipe', stderr: 'pipe' });
  const elmLog = elm.stdout.toString() + elm.stderr.toString();
  if (elm.exitCode !== 0) return { ok: false, log: elmLog };

  const ts = await Bun.build({
    entrypoints: ['platform/main.ts'],
    outdir: 'dist',
    target: 'browser',
    format: 'iife',
    minify: optimize,
    sourcemap: optimize ? 'none' : 'inline',
  });
  if (!ts.success) return { ok: false, log: ts.logs.map(String).join('\n') };

  copyFileSync('static/index.html', 'dist/index.html');
  return { ok: true, log: elmLog };
}

if (import.meta.main) {
  const result = await build(process.argv.includes('--optimize'));
  console.log(result.log.trim());
  if (!result.ok) process.exit(1);
  console.log('Built dist/');
}

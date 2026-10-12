// Dev server. Rebuilds Elm + TypeScript on every page load, so just refresh
// the browser after editing. Compile errors are shown in the page.
// Usage: bun scripts/serve.ts [--port 8000]
import { build } from './build';

const portArg = process.argv.indexOf('--port');
const port = portArg >= 0 ? Number(process.argv[portArg + 1]) : 8000;

const stripAnsi = (s: string): string => s.replace(/\x1b\[[0-9;]*m/g, '');
const escapeHtml = (s: string): string => s.replace(/&/g, '&amp;').replace(/</g, '&lt;');

Bun.serve({
  port,
  async fetch(req) {
    const path = new URL(req.url).pathname;
    if (path === '/' || path === '/index.html') {
      const result = await build(false);
      if (!result.ok) {
        const body = `<pre style="color:#fcfcfc;background:#101018;padding:16px;white-space:pre-wrap">${escapeHtml(stripAnsi(result.log))}</pre>`;
        return new Response(body, { status: 500, headers: { 'content-type': 'text/html' } });
      }
      return new Response(Bun.file('dist/index.html'));
    }
    const file = Bun.file(`dist${path}`);
    if (!path.includes('..') && (await file.exists())) return new Response(file);
    return new Response('Not found', { status: 404 });
  },
});

console.log(`Mount Diablo dev server: http://localhost:${port}  (refresh to rebuild)`);

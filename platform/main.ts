// Platform layer: draws Elm's frame commands on a low-res canvas, plays
// sounds/music and reports gamepads. All game logic lives in Elm (src/).
import { Audio, type Track } from './audio';
import { drawText, type Align } from './font';

const W = 384;
const H = 240;

// ---------- Port payload types (mirror src/Ports.elm + src/Render.elm) ----------

type DrawCmd =
  | ['r', number, number, number, number, string]
  | ['s', string, number, number, 0 | 1 | 2, 0 | 1, 0 | 1, number]
  | ['t', string, number, number, string, Align, number]
  | ['c', number, number, number, string, 0 | 1]
  | ['l', number, number, number, number, number, string]
  | ['a', number];

interface Frame {
  draws: DrawCmd[];
  sfx: string[];
  muted: boolean;
}

interface SpriteDef {
  id: string;
  rows: string[];
  pal: Record<string, string>;
}

interface SpriteSheet {
  palette: Record<string, string>;
  sprites: SpriteDef[];
}

interface PadState {
  left: boolean;
  right: boolean;
  up: boolean;
  down: boolean;
  start: boolean;
}

interface OutPort<T> {
  subscribe(cb: (value: T) => void): void;
}
interface InPort<T> {
  send(value: T): void;
}
interface ElmApp {
  ports: {
    frame: OutPort<Frame>;
    sprites: OutPort<SpriteSheet>;
    music: OutPort<Track>;
    gamepads: InPort<PadState[]>;
  };
}
declare global {
  interface Window {
    Elm: { Main: { init(opts: { node: HTMLElement; flags: { debug: boolean; titleBoss: number } }): ElmApp } };
  }
}

// ---------- Canvas setup ----------

const screenEl = document.getElementById('screen');
if (!(screenEl instanceof HTMLCanvasElement)) throw new Error('missing #screen canvas');
const canvas: HTMLCanvasElement = screenEl;
canvas.width = W;
canvas.height = H;
const ctxOrNull = canvas.getContext('2d');
if (!ctxOrNull) throw new Error('2d canvas unsupported');
const ctx: CanvasRenderingContext2D = ctxOrNull;
ctx.imageSmoothingEnabled = false;

function fit(): void {
  const scale = Math.min(window.innerWidth / W, window.innerHeight / H);
  canvas.style.width = `${W * scale}px`;
  canvas.style.height = `${H * scale}px`;
}
window.addEventListener('resize', fit);
fit();

// ---------- Sprites ----------

interface Baked {
  img: HTMLCanvasElement;
  flash: HTMLCanvasElement;
  w: number;
  h: number;
}
const sprites = new Map<string, Baked>();

function bake(def: SpriteDef, palette: Record<string, string>): Baked {
  const h = def.rows.length;
  const w = def.rows[0]?.length ?? 0;
  const make = (forceColor: string | null): HTMLCanvasElement => {
    const c = document.createElement('canvas');
    c.width = Math.max(1, w);
    c.height = Math.max(1, h);
    const g = c.getContext('2d');
    if (!g) return c;
    def.rows.forEach((row, y) => {
      for (let x = 0; x < row.length; x++) {
        const ch = row[x];
        if (ch === undefined || ch === '.') continue;
        const color = forceColor ?? def.pal[ch] ?? palette[ch];
        if (!color) continue;
        g.fillStyle = color;
        g.fillRect(x, y, 1, 1);
      }
    });
    return c;
  };
  return { img: make(null), flash: make('#fcfcfc'), w, h };
}

// ---------- Primitive raster helpers (pixel-perfect shapes) ----------

function pixelCircle(cx: number, cy: number, r: number, color: string, filled: boolean): void {
  ctx.fillStyle = color;
  const rr = Math.max(0, Math.round(r));
  if (filled) {
    for (let dy = -rr; dy <= rr; dy++) {
      const dx = Math.floor(Math.sqrt(rr * rr - dy * dy));
      ctx.fillRect(cx - dx, cy + dy, dx * 2 + 1, 1);
    }
    return;
  }
  const lim = Math.ceil(rr * Math.SQRT1_2);
  for (let x = 0; x <= lim; x++) {
    const y = Math.round(Math.sqrt(rr * rr - x * x));
    for (const [px, py] of [[x, y], [y, x], [-x, y], [-y, x], [x, -y], [y, -x], [-x, -y], [-y, -x]] as const) {
      ctx.fillRect(cx + px, cy + py, 1, 1);
    }
  }
}

function pixelLine(x1: number, y1: number, x2: number, y2: number, t: number, color: string): void {
  ctx.fillStyle = color;
  const steps = Math.max(Math.abs(x2 - x1), Math.abs(y2 - y1), 1);
  const o = Math.floor(t / 2);
  for (let i = 0; i <= steps; i++) {
    const x = Math.round(x1 + ((x2 - x1) * i) / steps);
    const y = Math.round(y1 + ((y2 - y1) * i) / steps);
    ctx.fillRect(x - o, y - o, t, t);
  }
}

// ---------- Frame rendering ----------

function render(draws: DrawCmd[]): void {
  ctx.globalAlpha = 1;
  ctx.fillStyle = '#000';
  ctx.fillRect(0, 0, W, H);
  for (const d of draws) {
    switch (d[0]) {
      case 'r': {
        const [, x, y, w, h, color] = d;
        if (w <= 0 || h <= 0) break;
        ctx.fillStyle = color;
        ctx.fillRect(x, y, w, h);
        break;
      }
      case 's': {
        const [, id, x, y, anchor, flip, flash, scale] = d;
        const s = sprites.get(id);
        if (!s) break;
        const w = s.w * scale;
        const h = s.h * scale;
        const left = anchor === 0 ? x : Math.round(x - w / 2);
        const top = anchor === 0 ? y : anchor === 1 ? y - h : Math.round(y - h / 2);
        const img = flash ? s.flash : s.img;
        if (flip) {
          ctx.save();
          ctx.translate(left + w, top);
          ctx.scale(-1, 1);
          ctx.drawImage(img, 0, 0, w, h);
          ctx.restore();
        } else {
          ctx.drawImage(img, left, top, w, h);
        }
        break;
      }
      case 't': {
        const [, text, x, y, color, align, scale] = d;
        drawText(ctx, text, x, y, color, align, scale);
        break;
      }
      case 'c': {
        const [, x, y, r, color, filled] = d;
        pixelCircle(x, y, r, color, filled === 1);
        break;
      }
      case 'l': {
        const [, x1, y1, x2, y2, t, color] = d;
        pixelLine(x1, y1, x2, y2, t, color);
        break;
      }
      case 'a': {
        ctx.globalAlpha = Math.max(0, Math.min(1, d[1]));
        break;
      }
    }
  }
  ctx.globalAlpha = 1;
}

// ---------- Boot ----------

const audio = new Audio();

// Keep the page from scrolling with arrows/space; start audio on first input.
window.addEventListener('keydown', (e) => {
  if (e.code.startsWith('Arrow') || e.code === 'Space') e.preventDefault();
  audio.unlock();
});
window.addEventListener('pointerdown', () => audio.unlock());

// A different level boss guards the title screen on every load (never the same twice in a row).
function pickTitleBoss(): number {
  const BOSSES = 11;
  let last = 0;
  try {
    last = Number(localStorage.getItem('titleBoss')) || 0;
  } catch {}
  // Draw from the bosses other than last time's, skipping over it.
  const pool = last >= 1 && last <= BOSSES ? BOSSES - 1 : BOSSES;
  let n = 1 + Math.floor(Math.random() * pool);
  if (pool < BOSSES && n >= last) n++;
  try {
    localStorage.setItem('titleBoss', String(n));
  } catch {}
  return n;
}

const mount = document.getElementById('elm');
if (!mount) throw new Error('missing #elm mount node');
const app = window.Elm.Main.init({
  node: mount,
  flags: { debug: new URLSearchParams(window.location.search).has('debug'), titleBoss: pickTitleBoss() },
});

app.ports.sprites.subscribe((sheet) => {
  for (const def of sheet.sprites) sprites.set(def.id, bake(def, sheet.palette));
});

app.ports.frame.subscribe((f) => {
  render(f.draws);
  audio.setMuted(f.muted);
  for (const name of f.sfx) audio.play(name);
});

app.ports.music.subscribe((t) => audio.setTrack(t));

// Gamepads (optional): pad N drives player N+1. Only send when changed.
let lastPads = '';
function pollPads(): void {
  const pads = navigator.getGamepads ? navigator.getGamepads() : [];
  const out: PadState[] = [];
  for (let i = 0; i < 4; i++) {
    const p = pads[i];
    if (!p || !p.connected) {
      out.push({ left: false, right: false, up: false, down: false, start: false });
      continue;
    }
    const b = (j: number): boolean => p.buttons[j]?.pressed ?? false;
    const ax = p.axes[0] ?? 0;
    out.push({
      left: b(14) || ax < -0.45,
      right: b(15) || ax > 0.45,
      up: b(12) || b(0),
      down: b(13) || b(1) || b(2),
      start: b(9),
    });
  }
  const key = JSON.stringify(out);
  if (key !== lastPads) {
    lastPads = key;
    app.ports.gamepads.send(out);
  }
  requestAnimationFrame(pollPads);
}
requestAnimationFrame(pollPads);

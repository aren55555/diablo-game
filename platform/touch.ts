// On-screen controls for touch devices. They act as player 1's keyboard: each
// button presses and releases the same key codes Elm already listens for, so
// the game logic needs no changes.

interface Button {
  code: string;
  label: string;
  cls: string;
}

const BUTTONS: Button[] = [
  { code: 'KeyA', label: '◀', cls: 'dir left' },
  { code: 'KeyD', label: '▶', cls: 'dir right' },
  { code: 'KeyS', label: 'ATK', cls: 'act atk' },
  { code: 'KeyW', label: 'JUMP', cls: 'act jump' },
  { code: 'Enter', label: 'START', cls: 'sys start' },
  { code: 'Escape', label: 'II', cls: 'sys pause' },
  { code: 'KeyM', label: '♪', cls: 'sys mute' },
];

// Extra reach around each button, in CSS px, so near-misses still count.
const SLOP = 14;

const CSS = `
#touch { position: fixed; inset: 0; z-index: 1; touch-action: none; user-select: none;
  -webkit-user-select: none; -webkit-touch-callout: none; font-family: system-ui, sans-serif; }
#touch .b { position: absolute; display: flex; align-items: center; justify-content: center;
  box-sizing: border-box; border: 2px solid rgba(252,252,252,0.35); background: rgba(252,252,252,0.12);
  color: rgba(252,252,252,0.8); font-weight: 700; --s: clamp(56px, 17vmin, 92px); }
#touch .b.on { background: rgba(252,208,0,0.45); border-color: #fcd000; color: #fff; }
#touch .dir, #touch .act { width: var(--s); height: var(--s); border-radius: 50%; font-size: calc(var(--s) * 0.32); }
#touch .dir { bottom: calc(env(safe-area-inset-bottom) + 18px); }
#touch .left { left: calc(env(safe-area-inset-left) + 14px); }
#touch .right { left: calc(env(safe-area-inset-left) + 26px + var(--s)); }
#touch .act { right: calc(env(safe-area-inset-right) + 14px); font-size: calc(var(--s) * 0.22); }
#touch .jump { bottom: calc(env(safe-area-inset-bottom) + 18px + var(--s) * 0.75); }
#touch .atk { bottom: calc(env(safe-area-inset-bottom) + 18px); right: calc(env(safe-area-inset-right) + 26px + var(--s)); }
#touch .sys { top: calc(env(safe-area-inset-top) + 10px); height: 34px; min-width: 44px; padding: 0 12px;
  border-radius: 17px; font-size: 13px; }
#touch .start { left: 50%; transform: translateX(-50%); }
#touch .pause { right: calc(env(safe-area-inset-right) + 12px); }
#touch .mute { right: calc(env(safe-area-inset-right) + 66px); }
`;

function isTouchDevice(): boolean {
  if (new URLSearchParams(window.location.search).has('touch')) return true;
  return window.matchMedia('(pointer: coarse)').matches || navigator.maxTouchPoints > 0;
}

function sendKey(type: 'keydown' | 'keyup', code: string): void {
  document.dispatchEvent(new KeyboardEvent(type, { code, key: code, bubbles: true }));
}

export function setupTouch(): void {
  if (!isTouchDevice()) return;

  const style = document.createElement('style');
  style.textContent = CSS;
  document.head.appendChild(style);

  const root = document.createElement('div');
  root.id = 'touch';
  const els = BUTTONS.map((b) => {
    const el = document.createElement('div');
    el.className = `b ${b.cls}`;
    el.textContent = b.label;
    root.appendChild(el);
    return el;
  });
  document.body.appendChild(root);

  // Every finger on screen, so you can hold a direction and jump at once, and
  // slide a thumb from one button to the next without lifting it.
  const fingers = new Map<number, { x: number; y: number }>();
  let held = new Set<number>();

  function hit(x: number, y: number): number {
    let best = -1;
    let bestD = Infinity;
    els.forEach((el, i) => {
      const r = el.getBoundingClientRect();
      if (x < r.left - SLOP || x > r.right + SLOP || y < r.top - SLOP || y > r.bottom + SLOP) return;
      const d = Math.hypot(x - (r.left + r.right) / 2, y - (r.top + r.bottom) / 2);
      if (d < bestD) {
        bestD = d;
        best = i;
      }
    });
    return best;
  }

  function sync(): void {
    const now = new Set<number>();
    for (const f of fingers.values()) {
      const i = hit(f.x, f.y);
      if (i >= 0) now.add(i);
    }
    for (const i of held) {
      if (now.has(i)) continue;
      const b = BUTTONS[i];
      if (b) sendKey('keyup', b.code);
      els[i]?.classList.remove('on');
    }
    for (const i of now) {
      if (held.has(i)) continue;
      const b = BUTTONS[i];
      if (b) sendKey('keydown', b.code);
      els[i]?.classList.add('on');
      navigator.vibrate?.(8);
    }
    held = now;
  }

  const track = (e: PointerEvent): void => {
    e.preventDefault();
    fingers.set(e.pointerId, { x: e.clientX, y: e.clientY });
    sync();
  };
  const release = (e: PointerEvent): void => {
    fingers.delete(e.pointerId);
    sync();
  };
  root.addEventListener('pointerdown', track);
  root.addEventListener('pointermove', (e) => {
    if (fingers.has(e.pointerId)) track(e);
  });
  root.addEventListener('pointerup', release);
  root.addEventListener('pointercancel', release);
  root.addEventListener('contextmenu', (e) => e.preventDefault());
  document.addEventListener('visibilitychange', () => {
    fingers.clear();
    sync();
  });
}

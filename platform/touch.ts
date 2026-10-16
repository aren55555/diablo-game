// On-screen controls for touch devices. They act as player 1's keyboard: each
// button presses and releases the same key codes Elm already listens for, so
// the game logic needs no changes.
//
// The game is played in landscape; in portrait a prompt asks to rotate. Two
// control styles, switched with the top-bar toggle and remembered:
//   buttons - round buttons in the bottom corners (default)
//   zones   - left half is a floating stick, right half is JUMP (top) / ATK (bottom)

type Controls = 'buttons' | 'zones';

interface Button {
  code: string;
  label: string;
  cls: string;
}

// `code: ''` marks the controls toggle, which is handled here rather than sent to Elm.
const BUTTONS: Button[] = [
  { code: 'KeyA', label: '◀', cls: 'dir left' },
  { code: 'KeyD', label: '▶', cls: 'dir right' },
  { code: 'KeyS', label: 'ATK', cls: 'act atk' },
  { code: 'KeyW', label: 'JUMP', cls: 'act jump' },
  { code: 'Enter', label: 'START', cls: 'sys start' },
  { code: 'Escape', label: 'II', cls: 'sys pause' },
  { code: 'KeyM', label: '♪', cls: 'sys mute' },
  { code: '', label: '', cls: 'sys ctrl' },
];
const LEFT = 0;
const RIGHT = 1;
const ATK = 2;
const JUMP = 3;
const CTRL = 7;

// Extra reach around each button, in CSS px, so near-misses still count.
const SLOP = 14;
// How far the stick must be pushed to register, and how far before its base follows the thumb.
const DEAD = 14;
const REACH = 44;

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
#touch .start { left: calc(env(safe-area-inset-left) + 14px); }
#touch .ctrl { left: calc(env(safe-area-inset-left) + 92px); }
#touch .pause { right: calc(env(safe-area-inset-right) + 12px); }
#touch .mute { right: calc(env(safe-area-inset-right) + 66px); }

#touch .zone, #touch .stick { display: none; }
#touch.zones .dir, #touch.zones .act { display: none; }
#touch.zones .zone { display: flex; position: absolute; right: 0; width: 50%; align-items: center;
  justify-content: center; box-sizing: border-box; color: rgba(252,252,252,0.5); font-weight: 700;
  font-size: 15px; border-left: 2px dashed rgba(252,252,252,0.18); transition: opacity 0.6s; }
#touch.zones .zone.jz { top: 0; height: 50%; border-bottom: 2px dashed rgba(252,252,252,0.18); }
#touch.zones .zone.az { bottom: 0; height: 50%; }
#touch.zones .zone.on { background: rgba(252,208,0,0.18); color: #fcd000; }
#touch.zones.idle .zone:not(.on) { opacity: 0.15; }
#touch.zones .stick.show { display: block; position: absolute; width: 96px; height: 96px; margin: -48px 0 0 -48px;
  border-radius: 50%; border: 2px solid rgba(252,252,252,0.35); background: rgba(252,252,252,0.08); }
#touch.zones .knob { position: absolute; left: 50%; top: 50%; width: 44px; height: 44px; margin: -22px 0 0 -22px;
  border-radius: 50%; background: rgba(252,208,0,0.5); }

#touch .rotate { display: none; }
@media (orientation: portrait) {
  #touch .rotate { display: flex; position: absolute; inset: 0; z-index: 2; flex-direction: column;
    align-items: center; justify-content: center; gap: 12px; background: rgba(0,0,0,0.85); color: #fcfcfc;
    font-weight: 700; font-size: 18px; }
  #touch .rotate span { font-size: 56px; color: #fcd000; }
}
`;

function isTouchDevice(): boolean {
  if (new URLSearchParams(window.location.search).has('touch')) return true;
  return window.matchMedia('(pointer: coarse)').matches || navigator.maxTouchPoints > 0;
}

function isPortrait(): boolean {
  return window.innerHeight > window.innerWidth;
}

function loadControls(): Controls {
  try {
    return localStorage.getItem('controls') === 'zones' ? 'zones' : 'buttons';
  } catch {
    return 'buttons';
  }
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

  const zone = (cls: string, label: string): HTMLDivElement => {
    const el = document.createElement('div');
    el.className = `zone ${cls}`;
    el.textContent = label;
    root.appendChild(el);
    return el;
  };
  const zones = { [JUMP]: zone('jz', 'JUMP'), [ATK]: zone('az', 'ATK') };
  const stick = document.createElement('div');
  stick.className = 'stick';
  const knob = document.createElement('div');
  knob.className = 'knob';
  stick.appendChild(knob);
  root.appendChild(stick);

  const rotate = document.createElement('div');
  rotate.className = 'rotate';
  rotate.innerHTML = '<span>↻</span>Turn your phone sideways';
  root.appendChild(rotate);
  document.body.appendChild(root);

  let controls = loadControls();
  function applyControls(): void {
    root.classList.toggle('zones', controls === 'zones');
    const ctrl = els[CTRL];
    if (ctrl) ctrl.textContent = controls === 'zones' ? 'ZONES' : 'BUTTONS';
  }
  applyControls();

  // Every finger on screen, so you can hold a direction and jump at once, and
  // slide a thumb from one button to the next without lifting it. ox/oy is
  // where the finger landed (the stick's base in zones mode).
  const fingers = new Map<number, { x: number; y: number; ox: number; oy: number }>();
  let held = new Set<number>();
  let idleTimer: ReturnType<typeof setTimeout> | undefined;

  function hit(x: number, y: number): number {
    let best = -1;
    let bestD = Infinity;
    els.forEach((el, i) => {
      const r = el.getBoundingClientRect();
      if (r.width === 0) return; // hidden in the current control style
      if (x < r.left - SLOP || x > r.right + SLOP || y < r.top - SLOP || y > r.bottom + SLOP) return;
      const d = Math.hypot(x - (r.left + r.right) / 2, y - (r.top + r.bottom) / 2);
      if (d < bestD) {
        bestD = d;
        best = i;
      }
    });
    return best;
  }

  // Zones mode: a finger that lands on the left half is a stick, the right half is two big buttons.
  function zoneHit(f: { x: number; y: number; ox: number; oy: number }): number {
    if (f.ox < window.innerWidth / 2) {
      const dx = f.x - f.ox;
      if (Math.abs(dx) > REACH) f.ox = f.x - Math.sign(dx) * REACH;
      return dx < -DEAD ? LEFT : dx > DEAD ? RIGHT : -1;
    }
    return f.y < window.innerHeight / 2 ? JUMP : ATK;
  }

  function sync(): void {
    const zoned = controls === 'zones';
    const now = new Set<number>();
    let stickFinger: { x: number; y: number; ox: number; oy: number } | null = null;
    // The rotate prompt covers everything in portrait, so no input gets through.
    for (const f of isPortrait() ? [] : fingers.values()) {
      let i = hit(f.x, f.y);
      if (i < 0 && zoned) {
        i = zoneHit(f);
        if (f.ox < window.innerWidth / 2) stickFinger ??= f;
      }
      if (i >= 0) now.add(i);
    }
    for (const i of held) {
      if (now.has(i)) continue;
      const b = BUTTONS[i];
      if (b?.code) sendKey('keyup', b.code);
      els[i]?.classList.remove('on');
      if (i === JUMP || i === ATK) zones[i].classList.remove('on');
    }
    for (const i of now) {
      if (held.has(i)) continue;
      const b = BUTTONS[i];
      if (b?.code) sendKey('keydown', b.code);
      els[i]?.classList.add('on');
      if (i === JUMP || i === ATK) zones[i].classList.add('on');
      if (i === CTRL) {
        controls = controls === 'zones' ? 'buttons' : 'zones';
        try {
          localStorage.setItem('controls', controls);
        } catch {}
        applyControls();
      }
      navigator.vibrate?.(8);
    }
    held = now;

    stick.classList.toggle('show', stickFinger !== null);
    if (stickFinger) {
      stick.style.left = `${stickFinger.ox}px`;
      stick.style.top = `${stickFinger.oy}px`;
      const dx = Math.max(-REACH, Math.min(REACH, stickFinger.x - stickFinger.ox));
      knob.style.transform = `translateX(${dx}px)`;
    }
    // Zone hints fade out once the player has been playing for a bit.
    if (fingers.size > 0) {
      root.classList.remove('idle');
      clearTimeout(idleTimer);
      idleTimer = setTimeout(() => root.classList.add('idle'), 2500);
    }
  }

  const track = (e: PointerEvent): void => {
    e.preventDefault();
    const prev = fingers.get(e.pointerId);
    fingers.set(e.pointerId, { x: e.clientX, y: e.clientY, ox: prev?.ox ?? e.clientX, oy: prev?.oy ?? e.clientY });
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
  // Rotating mid-press would otherwise leave keys stuck down under the prompt.
  const reset = (): void => {
    fingers.clear();
    sync();
  };
  document.addEventListener('visibilitychange', reset);
  window.matchMedia('(orientation: portrait)').addEventListener('change', reset);
}

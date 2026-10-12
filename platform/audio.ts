// 8-bit style sound effects and chiptune music, synthesized with WebAudio.

type Wave = OscillatorType;

export type Track = 'title' | 'climb' | 'boss' | 'win' | 'none';

interface Song {
  bpm: number;
  bass: readonly number[];
  lead: readonly number[];
}

// MIDI note numbers, 0 = rest. Eighth notes.
const SONGS: Record<Exclude<Track, 'none'>, Song> = {
  title: {
    bpm: 120,
    bass: [48, 0, 55, 0, 48, 0, 55, 0, 45, 0, 52, 0, 45, 0, 52, 0, 41, 0, 48, 0, 41, 0, 48, 0, 43, 0, 50, 0, 43, 0, 47, 0],
    lead: [72, 0, 76, 0, 79, 0, 76, 0, 72, 0, 76, 0, 81, 0, 79, 0, 77, 0, 76, 0, 74, 0, 72, 0, 74, 0, 0, 0, 71, 0, 0, 0],
  },
  climb: {
    bpm: 140,
    bass: [45, 0, 45, 0, 52, 0, 45, 0, 43, 0, 43, 0, 50, 0, 43, 0, 41, 0, 41, 0, 48, 0, 41, 0, 40, 0, 40, 0, 47, 0, 44, 0],
    lead: [69, 0, 72, 0, 76, 0, 74, 72, 71, 0, 67, 0, 71, 74, 0, 0, 69, 0, 72, 0, 77, 0, 76, 74, 72, 0, 71, 0, 68, 0, 0, 0],
  },
  boss: {
    bpm: 170,
    bass: [38, 38, 50, 38, 38, 50, 38, 50, 36, 36, 48, 36, 36, 48, 36, 48, 34, 34, 46, 34, 34, 46, 34, 46, 33, 33, 45, 33, 40, 40, 45, 44],
    lead: [62, 0, 65, 0, 69, 0, 68, 69, 0, 0, 67, 65, 64, 0, 0, 0, 62, 0, 65, 0, 70, 0, 69, 67, 65, 0, 64, 0, 61, 0, 0, 0],
  },
  win: {
    bpm: 132,
    bass: [48, 0, 55, 0, 48, 0, 55, 0, 53, 0, 60, 0, 53, 0, 60, 0, 55, 0, 62, 0, 55, 0, 62, 0, 48, 0, 55, 0, 48, 0, 0, 0],
    lead: [72, 76, 79, 84, 0, 79, 84, 0, 77, 81, 84, 89, 0, 84, 89, 0, 79, 83, 86, 91, 0, 86, 91, 0, 84, 0, 84, 0, 84, 0, 0, 0],
  },
};

const midiHz = (n: number): number => 440 * Math.pow(2, (n - 69) / 12);

export class Audio {
  private ctx: AudioContext | null = null;
  private master: GainNode | null = null;
  private musicGain: GainNode | null = null;
  private noiseBuf: AudioBuffer | null = null;
  private last = new Map<string, number>();
  private muted = false;
  private track: Track = 'none';
  private step = 0;
  private nextNoteTime = 0;

  unlock(): void {
    if (!this.ctx) {
      const Ctor = window.AudioContext;
      if (!Ctor) return;
      const ctx = new Ctor();
      this.ctx = ctx;
      this.master = ctx.createGain();
      this.master.gain.value = this.muted ? 0 : 0.3;
      this.master.connect(ctx.destination);
      this.musicGain = ctx.createGain();
      this.musicGain.gain.value = 0.5;
      this.musicGain.connect(this.master);
      const len = ctx.sampleRate;
      this.noiseBuf = ctx.createBuffer(1, len, ctx.sampleRate);
      const d = this.noiseBuf.getChannelData(0);
      // 8-bit flavoured noise: sample-and-hold steps
      let v = 0;
      for (let i = 0; i < len; i++) {
        if (i % 4 === 0) v = Math.random() * 2 - 1;
        d[i] = v;
      }
      this.startSequencer();
    }
    if (this.ctx.state === 'suspended') void this.ctx.resume();
  }

  setMuted(m: boolean): void {
    if (m === this.muted) return;
    this.muted = m;
    if (this.master) this.master.gain.value = m ? 0 : 0.3;
  }

  setTrack(t: Track): void {
    if (t === this.track) return;
    this.track = t;
    this.step = 0;
    if (this.ctx) this.nextNoteTime = this.ctx.currentTime + 0.05;
  }

  private startSequencer(): void {
    if (!this.ctx) return;
    this.nextNoteTime = this.ctx.currentTime + 0.05;
    window.setInterval(() => this.schedule(), 25);
  }

  private schedule(): void {
    const ctx = this.ctx;
    if (!ctx || !this.musicGain) return;
    if (this.track === 'none') {
      this.nextNoteTime = ctx.currentTime + 0.05;
      return;
    }
    const song = SONGS[this.track];
    const stepDur = 60 / song.bpm / 2;
    while (this.nextNoteTime < ctx.currentTime + 0.12) {
      const i = this.step % song.lead.length;
      const lead = song.lead[i] ?? 0;
      const bass = song.bass[i] ?? 0;
      if (lead) this.note(midiHz(lead), stepDur * 0.9, 'square', 0.05, this.nextNoteTime, this.musicGain);
      if (bass) this.note(midiHz(bass), stepDur * 0.95, 'triangle', 0.12, this.nextNoteTime, this.musicGain);
      if (this.track === 'boss' && i % 2 === 0) this.noiseAt(0.04, 0.05, 6000, this.nextNoteTime, this.musicGain);
      this.nextNoteTime += stepDur;
      this.step++;
    }
  }

  private note(f: number, dur: number, type: Wave, vol: number, t: number, dest: AudioNode): void {
    const ctx = this.ctx;
    if (!ctx) return;
    const o = ctx.createOscillator();
    const g = ctx.createGain();
    o.type = type;
    o.frequency.setValueAtTime(f, t);
    g.gain.setValueAtTime(vol, t);
    g.gain.setValueAtTime(vol, t + dur * 0.7);
    g.gain.linearRampToValueAtTime(0, t + dur);
    o.connect(g);
    g.connect(dest);
    o.start(t);
    o.stop(t + dur + 0.01);
  }

  private tone(f: number, dur: number, type: Wave = 'square', vol = 0.12, f2: number | null = null, delay = 0): void {
    const ctx = this.ctx;
    if (!ctx || !this.master) return;
    const t = ctx.currentTime + delay;
    const o = ctx.createOscillator();
    const g = ctx.createGain();
    o.type = type;
    o.frequency.setValueAtTime(f, t);
    if (f2) o.frequency.exponentialRampToValueAtTime(f2, t + dur);
    g.gain.setValueAtTime(vol, t);
    g.gain.exponentialRampToValueAtTime(0.0001, t + dur);
    o.connect(g);
    g.connect(this.master);
    o.start(t);
    o.stop(t + dur + 0.02);
  }

  private noiseAt(dur: number, vol: number, freq: number, t: number, dest: AudioNode, freq2: number | null = null): void {
    const ctx = this.ctx;
    if (!ctx || !this.noiseBuf) return;
    const s = ctx.createBufferSource();
    const f = ctx.createBiquadFilter();
    const g = ctx.createGain();
    s.buffer = this.noiseBuf;
    f.type = 'lowpass';
    f.frequency.setValueAtTime(freq, t);
    if (freq2) f.frequency.exponentialRampToValueAtTime(freq2, t + dur);
    g.gain.setValueAtTime(vol, t);
    g.gain.exponentialRampToValueAtTime(0.0001, t + dur);
    s.connect(f);
    f.connect(g);
    g.connect(dest);
    s.start(t, Math.random() * 0.5);
    s.stop(t + dur + 0.02);
  }

  private noise(dur: number, vol: number, freq: number, freq2: number | null = null, delay = 0): void {
    if (!this.ctx || !this.master) return;
    this.noiseAt(dur, vol, freq, this.ctx.currentTime + delay, this.master, freq2);
  }

  private fart(): void {
    const ctx = this.ctx;
    if (!ctx || !this.master) return;
    const t = ctx.currentTime;
    const o = ctx.createOscillator();
    const g = ctx.createGain();
    const lfo = ctx.createOscillator();
    const lg = ctx.createGain();
    o.type = 'square';
    o.frequency.setValueAtTime(90 + Math.random() * 40, t);
    o.frequency.exponentialRampToValueAtTime(45 + Math.random() * 15, t + 0.75);
    lfo.type = 'square';
    lfo.frequency.value = 22 + Math.random() * 12;
    lg.gain.value = 0.08;
    lfo.connect(lg);
    lg.connect(g.gain);
    g.gain.setValueAtTime(0.1, t);
    g.gain.linearRampToValueAtTime(0, t + 0.8);
    o.connect(g);
    g.connect(this.master);
    o.start(t);
    lfo.start(t);
    o.stop(t + 0.85);
    lfo.stop(t + 0.85);
    this.noise(0.6, 0.05, 400, 150);
  }

  private seq(notes: readonly number[], dur: number, type: Wave, vol: number, gap: number): void {
    notes.forEach((n, i) => this.tone(midiHz(n), dur, type, vol, null, i * gap));
  }

  play(name: string): void {
    if (!this.ctx || this.muted) return;
    const now = performance.now();
    const prev = this.last.get(name);
    if (prev !== undefined && now - prev < 45) return;
    this.last.set(name, now);
    switch (name) {
      case 'jump': this.tone(300, 0.12, 'square', 0.06, 620); break;
      case 'djump': this.tone(480, 0.14, 'square', 0.06, 980); break;
      case 'punch': this.noise(0.08, 0.3, 1400, 300); break;
      case 'swish': this.noise(0.12, 0.2, 3500, 700); break;
      case 'sword': this.tone(900, 0.18, 'square', 0.06, 1900); this.noise(0.12, 0.12, 5000, 1500); break;
      case 'gun': this.noise(0.05, 0.2, 2600, 500); this.tone(170, 0.04, 'square', 0.05, 80); break;
      case 'rocket': this.noise(0.35, 0.22, 500, 2200); break;
      case 'boom': this.noise(0.7, 0.5, 900, 60); this.tone(90, 0.45, 'triangle', 0.35, 30); break;
      case 'fart': this.fart(); break;
      case 'laser': this.tone(1500, 0.2, 'square', 0.05, 180); break;
      case 'hurt': this.tone(240, 0.18, 'square', 0.1, 110); break;
      case 'coin': this.tone(988, 0.07, 'square', 0.05); this.tone(1319, 0.14, 'square', 0.05, null, 0.07); break;
      case 'diamond': this.seq([84, 88, 91, 96], 0.12, 'square', 0.05, 0.05); break;
      case 'heart': this.seq([72, 76, 79, 84], 0.12, 'triangle', 0.12, 0.06); break;
      case 'pickup': this.seq([67, 72, 76, 79], 0.1, 'square', 0.07, 0.05); break;
      case 'enemyDie': this.noise(0.2, 0.2, 2000, 200); this.tone(500, 0.2, 'square', 0.06, 120); break;
      case 'ko': this.seq([69, 66, 63, 57], 0.18, 'square', 0.08, 0.12); break;
      case 'roar': this.noise(1.2, 0.4, 350, 90); this.tone(80, 1.1, 'sawtooth', 0.12, 45); break;
      case 'clear': this.seq([72, 76, 79, 84, 79, 84], 0.22, 'square', 0.07, 0.11); break;
      case 'breath': this.noise(1.6, 0.25, 1200, 300); break;
      case 'freeze': this.tone(1800, 0.3, 'triangle', 0.05, 2600); break;
      case 'zap': this.tone(220, 1.4, 'sawtooth', 0.05, 160); this.tone(330, 1.4, 'square', 0.03, 240); break;
      case 'stomp': this.tone(90, 0.3, 'triangle', 0.35, 35); this.noise(0.3, 0.3, 500, 80); break;
      case 'select': this.tone(660, 0.08, 'square', 0.07); this.tone(990, 0.1, 'square', 0.07, null, 0.08); break;
      case 'shoot': this.tone(600, 0.12, 'square', 0.05, 300); break;
      case 'crumble': this.noise(0.4, 0.15, 800, 200); break;
      case 'win': this.seq([72, 76, 79, 84, 88, 91, 96], 0.3, 'square', 0.08, 0.13); break;
    }
  }
}

/* Grid and smart-guide snapping for a dragged selection.  Board units
   throughout; SCALE is screen pixels per board unit.  Boxes are [x, y, w, h]. */
export const GRID = 20;
const CAPTURE = 6, EPS = 0.01;

/* The two dot layers at SCALE: the fine step is GRID times the smallest power
   of five that keeps dots 8 px apart, the strong step five times that.  A
   layer's opacity and radius depend only on its own screen spacing, so the
   dots do not pop when the step changes. */
export function gridLevels(scale) {
  let step = GRID;
  while (step * scale < 8) step *= 5;
  return [step, step * 5].map((size) => {
    const px = size * scale;
    return { step: size, px, alpha: Math.max(0, Math.min(1, (px - 8) / 8)),
      radius: 0.7 + 0.5 * Math.max(0, Math.min(1, (px - 40) / 60)) };
  });
}

/* The neighbours of [POS, POS + SIZE] on axis I among TARGETS that share its
   cross span [C0, C1]: the two nearest before and the two nearest after. */
function neighbours(pos, size, c0, c1, targets, i, slack) {
  let p1, p2, n1, n2;
  for (const t of targets) {
    if (t[1 - i] + t[3 - i] <= c0 || t[1 - i] >= c1) continue;
    const end = t[i] + t[i + 2];
    if (end <= pos + slack) {
      if (!p1 || end > p1[i] + p1[i + 2]) [p1, p2] = [t, p1];
      else if (!p2 || end > p2[i] + p2[i + 2]) p2 = t;
    } else if (t[i] >= pos + size - slack) {
      if (!n1 || t[i] < n1[i]) [n1, n2] = [t, n1];
      else if (!n2 || t[i] < n2[i]) n2 = t;
    }
  }
  return [p1, p2, n1, n2];
}

/* Where [POS, POS + SIZE] lands on axis I (0 for x, 1 for y). */
function snapAxis(pos, size, c0, c1, targets, view, i, o) {
  if (o.bypass) return pos;
  if (o.guides) {
    const reach = CAPTURE / o.scale;
    let best = Infinity, shift = 0, rank = 0;
    // The nearest wins; objects beat equal spacing, which beats the view.
    const offer = (delta, kind) => {
      const d = Math.abs(delta);
      if (d <= reach && (d < best - EPS || (d < best + EPS && kind < rank))) [best, shift, rank] = [d, delta, kind];
    };
    for (const t of targets) {
      // Like with like, and an edge to the facing edge.
      offer(t[i] - pos, 0); offer(t[i] + t[i + 2] - pos, 0);
      offer(t[i] - pos - size, 0); offer(t[i] + t[i + 2] - pos - size, 0);
      offer(t[i] + t[i + 2] / 2 - pos - size / 2, 0);
    }
    const [p1, p2, n1, n2] = neighbours(pos, size, c0, c1, targets, i, reach);
    const end = (t) => t[i] + t[i + 2];
    if (p1 && n1) offer(end(p1) + (n1[i] - end(p1) - size) / 2 - pos, 1);
    if (p1 && p2 && p1[i] > end(p2)) offer(end(p1) + p1[i] - end(p2) - pos, 1);
    if (n1 && n2 && n2[i] > end(n1)) offer(n1[i] - (n2[i] - end(n1)) - size - pos, 1);
    offer(view[i] - pos, 2); offer(view[i] + view[i + 2] - pos - size, 2);
    offer(view[i] + view[i + 2] / 2 - pos - size / 2, 2);
    if (best < Infinity) return pos + shift;
  }
  return o.grid ? Math.round(pos / GRID) * GRID : pos;
}

/* The guides that hold exactly with [POS, POS + SIZE] on axis I. */
function report(pos, size, c0, c1, targets, view, i, out) {
  const same = (a, b) => Math.abs(a - b) < EPS, vertical = i === 0;
  const mine = [pos, pos + size / 2, pos + size], found = new Map();
  for (const t of targets) {
    const theirs = [t[i], t[i] + t[i + 2] / 2, t[i] + t[i + 2]];
    for (const [a, b] of [[0, 0], [0, 2], [2, 0], [2, 2], [1, 1]]) {
      if (!same(mine[a], theirs[b])) continue;
      const line = found.get(a) || { kind: 'object', vertical, at: mine[a], from: c0, to: c1 };
      line.from = Math.min(line.from, t[1 - i]);
      line.to = Math.max(line.to, t[1 - i] + t[3 - i]);
      found.set(a, line);
    }
  }
  // Same-size neighbours match on every anchor; the edge lines say so
  // without striking through their labels.
  if (found.size === 3) found.delete(1);
  out.lines.push(...found.values());
  [view[i], view[i] + view[i + 2] / 2, view[i] + view[i + 2]].forEach((at, a) => {
    if (same(mine[a], at)) out.lines.push({ kind: a === 1 ? 'view-centre' : 'view-edge', vertical, at, far: a === 2 });
  });
  const me = [0, 0, 0, 0];
  me[i] = pos; me[i + 2] = size; me[1 - i] = c0; me[3 - i] = c1 - c0;
  const [p1, p2, n1, n2] = neighbours(pos, size, c0, c1, targets, i, EPS);
  const gap = (l, r) => {
    const low = Math.max(l[1 - i], r[1 - i]), high = Math.min(l[1 - i] + l[3 - i], r[1 - i] + r[3 - i]);
    return { along: i, from: l[i] + l[i + 2], to: r[i], cross: (low + high) / 2, size: r[i] - l[i] - l[i + 2] };
  };
  for (const [a, b, c] of [[p1, me, n1], [p2, p1, me], [me, n1, n2]]) {
    if (!a || !b || !c) continue;
    const first = gap(a, b), second = gap(b, c);
    if (first.size > EPS && same(first.size, second.size)) { out.gaps.push(first, second); break; }
  }
}

/* Snap a moving selection.  BOX is its extent where the pointer puts it,
   TARGETS the stationary boxes it may align with, VIEW the visible canvas.
   O holds scale, grid, guides and bypass.  Each axis is decided on its own:
   a guide within CAPTURE screen pixels wins, then the grid.  Of two guides
   equally near, the earlier target's wins, so callers pass TARGETS in a
   stable order.  Returns the
   extent's x and y with the lines and equal gaps that then hold exactly. */
export function snapMove(box, targets, view, o) {
  const seen = o.guides ? targets.filter((t) => t[0] < view[0] + view[2] && t[0] + t[2] > view[0]
    && t[1] < view[1] + view[3] && t[1] + t[3] > view[1]) : [];
  const [, , w, h] = box;
  const x = snapAxis(box[0], w, box[1], box[1] + h, seen, view, 0, o);
  const y = snapAxis(box[1], h, box[0], box[0] + w, seen, view, 1, o);
  const out = { x, y, lines: [], gaps: [] };
  if (o.guides) {
    report(x, w, y, y + h, seen, view, 0, out);
    report(y, h, x, x + w, seen, view, 1, out);
  }
  return out;
}

/* SVG for the guides of RESULT, a snapMove result for a selection WxH. */
export function guidesSVG(result, [, , w, h], view, scale) {
  const px = 1 / scale, out = [];
  const seg = (x1, y1, x2, y2, dash = '') => ['case', `line${dash}`].map((name) =>
    `<line class="${name}" x1="${x1}" y1="${y1}" x2="${x2}" y2="${y2}" vector-effect="non-scaling-stroke"/>`).join('');
  const pill = (x, y, label) => {
    const width = (label.length * 6.2 + 10) * px, height = 15 * px;
    return `<rect class="pill" x="${x - width / 2}" y="${y - height / 2}" width="${width}" height="${height}" rx="${3 * px}"/>`
      + `<text x="${x}" y="${y}" style="font-size:${10 * px}px">${label}</text>`;
  };
  const tag = 'Centre of your view';
  for (const l of result.lines) {
    if (l.kind === 'object') {
      const a = l.from - 8 * px, b = l.to + 8 * px;
      out.push(l.vertical ? seg(l.at, a, l.at, b) : seg(a, l.at, b, l.at));
    } else if (l.kind === 'view-centre') {
      out.push(l.vertical ? seg(l.at, view[1], l.at, view[1] + view[3], ' dash') + pill(l.at, result.y + h + 22 * px, tag)
        : seg(view[0], l.at, view[0] + view[2], l.at, ' dash') + pill(result.x + w + 70 * px, l.at, tag));
    } else {
      const t = 3 * px, at = l.far ? l.at - t : l.at;
      out.push(l.vertical ? `<rect class="edge" x="${at}" y="${view[1]}" width="${t}" height="${view[3]}"/>`
        : `<rect class="edge" x="${view[0]}" y="${at}" width="${view[2]}" height="${t}"/>`);
    }
  }
  for (const g of result.gaps) {
    const cap = 5 * px, mid = (g.from + g.to) / 2, label = String(Math.round(g.size * 10) / 10), room = g.size * scale >= 28;
    if (g.along === 0) out.push(seg(g.from, g.cross, g.to, g.cross), seg(g.from, g.cross - cap, g.from, g.cross + cap),
      seg(g.to, g.cross - cap, g.to, g.cross + cap), room ? pill(mid, g.cross - 14 * px, label) : '');
    else out.push(seg(g.cross, g.from, g.cross, g.to), seg(g.cross - cap, g.from, g.cross + cap, g.from),
      seg(g.cross - cap, g.to, g.cross + cap, g.to), room ? pill(g.cross + 18 * px, mid, label) : '');
  }
  return out.join('');
}

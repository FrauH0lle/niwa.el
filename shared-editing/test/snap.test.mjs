import test from 'node:test';
import assert from 'node:assert/strict';
import { gridLevels, guidesSVG, snapMove } from '../snap.mjs';

const view = [0, 0, 1440, 900], on = { scale: 1, grid: true, guides: true, bypass: false };
const A = [100, 120, 160, 80], B = [320, 120, 160, 80], odd = [247, 400, 113, 67];
const at = (r) => [r.x, r.y];

test('the grid takes the nearest intersection, also below zero', () => {
  assert.deepEqual(at(snapMove([-213, 647, 50, 50], [], view, { ...on, guides: false })), [-220, 640]);
});

test('a guide within six screen pixels beats the grid, per axis', () => {
  const r = snapMove([251, 600, 20, 50], [odd], view, on);
  assert.deepEqual(at(r), [247, 600]);
  assert.ok(r.lines.some((l) => l.kind === 'object' && l.vertical && l.at === 247));
  assert.deepEqual(at(snapMove([212, 603, 20, 50], [odd], view, on)), [220, 600]);
  // Ten units are five pixels at half zoom.
  assert.equal(snapMove([257, 600, 20, 50], [odd], view, { ...on, scale: 0.5 }).x, 247);
  assert.equal(snapMove([257, 600, 20, 50], [odd], view, on).x, 260);
});

test('bypass leaves the position alone', () => {
  assert.deepEqual(at(snapMove([251, 603, 20, 50], [odd], view, { ...on, bypass: true })), [251, 603]);
});

test('equal spacing continues a row and sits between two neighbours', () => {
  let r = snapMove([537, 123, 160, 80], [A, B], view, on);
  assert.deepEqual(at(r), [540, 120]);
  assert.deepEqual(r.gaps.map((g) => [g.along, g.from, g.to, g.size]), [[0, 260, 320, 60], [0, 480, 540, 60]]);
  // Same-size boxes show their two edge lines, not the centre line.
  assert.deepEqual(r.lines.filter((l) => !l.vertical).map((l) => l.at), [120, 200]);
  r = snapMove([403, 303, 70, 40], [[100, 300, 200, 40], [580, 300, 200, 40]], view, { ...on, grid: false });
  assert.equal(r.x, 405);
  assert.equal(r.gaps.length, 2);
  r = snapMove([700, 297, 40, 30], [[700, 100, 40, 50], [700, 200, 40, 50]], view, { ...on, grid: false });
  assert.equal(r.y, 300);
  assert.deepEqual(r.gaps.map((g) => g.along), [1, 1]);
});

test('the view offers its centre and edges', () => {
  let r = snapMove([618, 500, 200, 40], [], view, { ...on, grid: false });
  assert.equal(r.x, 620);
  assert.deepEqual(r.lines.map((l) => l.kind), ['view-centre']);
  r = snapMove([1243, 500, 200, 40], [], view, { ...on, grid: false });
  assert.equal(r.x, 1240);
  assert.ok(r.lines[0].kind === 'view-edge' && r.lines[0].far);
});

test('objects outside the view do not pull', () => {
  assert.equal(snapMove([251, 600, 20, 50], [[247, 5000, 113, 67]], view, { ...on, grid: false }).x, 251);
});

test('equal distances go to the first target', () => {
  const r = snapMove([103, 600, 20, 20], [[100, 300, 50, 50], [106, 400, 50, 50]], view, { ...on, grid: false });
  assert.equal(r.x, 100);
});

test('grid levels coarsen by five and fade in', () => {
  assert.deepEqual(gridLevels(1).map((l) => l.step), [20, 100]);
  assert.deepEqual(gridLevels(4).map((l) => l.step), [20, 100]);
  assert.deepEqual(gridLevels(0.25).map((l) => l.step), [100, 500]);
  assert.deepEqual(gridLevels(0.05).map((l) => l.step), [500, 2500]);
  assert.deepEqual(gridLevels(1).map((l) => [l.alpha, l.radius]), [[1, 0.7], [1, 1.2]]);
  assert.equal(gridLevels(0.5)[0].alpha, 0.25);
});

test('guides draw lines, gap bars with their size, and the view tag', () => {
  const row = guidesSVG(snapMove([537, 123, 160, 80], [A, B], view, on), [0, 0, 160, 80], view, 1);
  assert.equal(row.match(/class="pill"/g).length, 2);
  assert.match(row, />60</);
  const narrow = guidesSVG(snapMove([537, 123, 160, 80], [A, B], view, { ...on, scale: 0.4 }), [0, 0, 160, 80], view, 0.4);
  assert.doesNotMatch(narrow, /class="pill"/);
  const centre = guidesSVG(snapMove([618, 500, 200, 40], [], view, { ...on, grid: false }), [0, 0, 200, 40], view, 1);
  assert.match(centre, /class="line dash"/);
  assert.match(centre, /Centre of your view/);
});

import test from 'node:test';
import assert from 'node:assert/strict';
import {editorFixture} from './editor-fixture.mjs';

const read = (page) => page.evaluate(async () => (await window.apply({action:'read'})).content);
const saved = (frame) => frame.locator('html[data-save-state=saved]').waitFor({state:'attached'});
const sent = (page, type) => page.evaluate(type => window.messages.filter(m => m.type === type), type);
const box = (content, id) => { const e = content.find(e => e.id === id); return [e.x, e.y, e.width, e.height]; };
const rect = (id, x, y, width, height) => ({id, type:'rectangle', x, y, width, height});
/* Screen position of board point [x, y] in the page. */
async function at(page, frame, x, y) {
  const offset = await page.locator('iframe').boundingBox();
  const [px, py] = await frame.locator('#canvas').evaluate((canvas, [x, y]) => {
    const p = new DOMPoint(x, y).matrixTransform(canvas.getScreenCTM());
    return [p.x, p.y];
  }, [x, y]);
  return [offset.x + px, offset.y + py];
}
/* Press at board point FROM and move the pointer to TO, in two samples. */
async function dragTo(page, frame, from, to) {
  await page.mouse.move(...await at(page, frame, ...from));
  await page.mouse.down();
  await page.mouse.move(...await at(page, frame, (from[0] + to[0]) / 2, (from[1] + to[1]) / 2));
  await page.mouse.move(...await at(page, frame, ...to));
}
const drop = async (page, frame) => { await page.mouse.up(); await saved(frame); };
/* The box the last preview sent to peers gives element ID. */
const peerBox = async (page, id) => (await sent(page, 'presence')).filter(m => m.preview).at(-1).preview.shapes.find(s => s.id === id).box;
/* Where the lone selected element is drawn now: its outline, less the gap. */
const shown = (frame) => frame.locator('#selection > rect').first().evaluate(r => {
  const gap = 4 / document.getElementById('canvas').getScreenCTM().a;
  return [+r.getAttribute('x') + gap, +r.getAttribute('y') + gap, +r.getAttribute('width') - 2 * gap, +r.getAttribute('height') - 2 * gap];
});
/* The choices sent to the host page so far, once COUNT have arrived. */
const views = async (page, count) => {
  await page.waitForFunction(count => window.messages.filter(m => m.type === 'board-view').length >= count, count);
  return (await sent(page, 'board-view')).map(m => m.view);
};
const near = (actual, expected, slack = 0.01) => actual.forEach((v, i) =>
  assert.ok(Math.abs(v - expected[i]) <= slack, `${actual} is not within ${slack} of ${expected}`));

test('board grid, snap to grid and smart guides', async (t) => {
  const {open} = await editorFixture(t);
  const odd = rect('odd', 247, 400, 113, 67);

  await t.test('the grid and snapping choices stay in this browser', async () => {
    const {page, frame} = await open({content:[rect('m', 103, 107, 90, 50)]});
    assert.ok(await frame.locator('#board-grid').isVisible());
    await frame.locator('#view-menu > summary').click();
    assert.deepEqual(await frame.locator('#view-menu input').evaluateAll(all => all.map(i => [i.id, i.checked])),
      [['view-grid', true], ['view-snap', false], ['view-guides', true]]);
    await frame.getByLabel('Snap to grid').check();
    assert.equal(await frame.locator('#board-announce').textContent(), 'Snap to grid on');
    assert.equal(await frame.locator('#view-menu > summary').getAttribute('aria-label'), 'Grid and snapping, snap to grid on');
    assert.equal(await frame.locator('#view-menu').getAttribute('data-snap'), 'true');
    // The keys work from the canvas: G hides the grid, S turns the guides off.
    await page.mouse.click(...await at(page, frame, 400, 300));
    await page.keyboard.press('g');
    assert.ok(await frame.locator('#board-grid').isHidden());
    await page.keyboard.press('s');
    await page.keyboard.press('Shift+G');
    assert.equal(await frame.locator('#board-announce').textContent(), 'Snap to grid off');
    assert.deepEqual(await views(page, 4), [
      {grid:true, snap:true, guides:true}, {grid:false, snap:true, guides:true},
      {grid:false, snap:true, guides:false}, {grid:false, snap:false, guides:false}]);
    // Nothing reached the board: no edit, no undo step, no word to peers.
    await frame.locator('#undo').click();
    assert.ok(await frame.locator('#board-grid').isHidden());
    assert.equal((await sent(page, 'request')).filter(m => m.args.action === 'update').length, 0);
    assert.equal((await page.evaluate(() => window.apply({action:'read'}))).revision, 1);
    assert.ok((await sent(page, 'presence')).every(m => !('view' in m) && !('boardView' in m)));
    await page.close();

    const again = await open({content:[rect('m', 103, 107, 90, 50)], boardView:{grid:false, snap:true, guides:false}});
    assert.ok(await again.frame.locator('#board-grid').isHidden());
    assert.deepEqual(await again.frame.locator('#view-menu input').evaluateAll(all => all.map(i => i.checked)), [false, true, false]);
    assert.equal((await sent(again.page, 'board-view')).length, 0);
    await again.page.close();
  });

  await t.test('the dots sit on the board grid as the view zooms', async () => {
    const {page, frame} = await open({content:[rect('m', 100, 100, 200, 100)]});
    const grid = () => frame.locator('#board-grid').evaluate(dots => {
      const m = document.getElementById('canvas').getScreenCTM(), r = dots.getBoundingClientRect();
      const px = name => parseFloat(dots.style.getPropertyValue(name));
      // Where the fine layer's dot nearest the board origin lands, against the origin itself.
      return {scale:m.a, size:px('--grid-size1'), strong:px('--grid-size2'),
        off:[px('--grid-x1') + px('--grid-size1') / 2 - (m.e - r.left), px('--grid-y1') + px('--grid-size1') / 2 - (m.f - r.top)]};
    });
    let g = await grid();
    near([g.size, g.strong], [20 * g.scale, 100 * g.scale]);
    near(g.off, [0, 0]);
    for (let i = 0; i < 6; i++) await frame.getByRole('button', {name:'Zoom out'}).click();
    g = await grid();
    assert.ok(g.scale < 0.4, `zoomed out to ${g.scale}`);
    near([g.size, g.strong], [100 * g.scale, 500 * g.scale]);
    near(g.off, [0, 0]);
    await page.close();
  });

  await t.test('snap to grid moves the selection to the nearest grid point', async () => {
    const {page, frame} = await open({content:[rect('m', 103, 107, 90, 50)], boardView:{snap:true, guides:false}});
    await dragTo(page, frame, [140, 130], [173, 171]);
    assert.equal(await frame.locator('#selection .guides').count(), 0);
    await drop(page, frame);
    assert.deepEqual(box(await read(page), 'm'), [140, 140, 90, 50]);
    assert.deepEqual(await peerBox(page, 'm'), [140, 140, 90, 50]);
    // A press that moves under four pixels is the small move it always was.
    await page.mouse.move(...await at(page, frame, 180, 160));
    await page.mouse.down();
    const [x, y] = await at(page, frame, 180, 160);
    await page.mouse.move(x + 2, y);
    await drop(page, frame);
    const scale = await frame.locator('#canvas').evaluate(c => c.getScreenCTM().a);
    near(box(await read(page), 'm'), [140 + 2 / scale, 140, 90, 50]);
    // A click without movement moves nothing.
    await page.mouse.down();
    await page.mouse.up();
    near(box(await read(page), 'm'), [140 + 2 / scale, 140, 90, 50]);
    await page.close();
  });

  await t.test('a guide within six pixels beats the grid, and only there', async () => {
    const {page, frame} = await open({content:[odd, rect('m', 100, 600, 20, 50)], boardView:{snap:true}});
    // The left edge comes to x = 251, four units from the off-grid edge at 247.
    await dragTo(page, frame, [110, 625], [261, 628]);
    assert.ok(await frame.locator('#selection .guides .line').count() > 0);
    await drop(page, frame);
    assert.equal(await frame.locator('#selection .guides').count(), 0);
    assert.deepEqual(box(await read(page), 'm'), [247, 600, 20, 50]);
    assert.deepEqual(await peerBox(page, 'm'), [247, 600, 20, 50]);
    // Thirteen units away the grid decides again.
    await dragTo(page, frame, [257, 625], [222, 625]);
    await drop(page, frame);
    assert.deepEqual(box(await read(page), 'm'), [220, 600, 20, 50]);
    await page.close();
  });

  await t.test('Alt places the selection freely', async () => {
    const {page, frame} = await open({content:[odd, rect('m', 100, 600, 20, 50)], boardView:{snap:true}});
    const scale = await frame.locator('#canvas').evaluate(c => c.getScreenCTM().a);
    const from = await at(page, frame, 110, 625), to = [from[0] + Math.round(151 * scale), from[1] + Math.round(3 * scale)];
    const free = [100 + (to[0] - from[0]) / scale, 600 + (to[1] - from[1]) / scale, 20, 50];
    await page.mouse.move(...from);
    await page.mouse.down();
    await page.mouse.move(...to);
    near(await shown(frame), [247, 600, 20, 50]);
    // Pressing Alt takes effect at once, without another pointer sample.
    await page.keyboard.down('Alt');
    near(await shown(frame), free);
    assert.equal(await frame.locator('#selection .guides .line').count(), 0);
    await page.keyboard.up('Alt');
    near(await shown(frame), [247, 600, 20, 50]);
    await page.keyboard.down('Alt');
    await page.mouse.move(to[0], to[1] + 1);
    await page.mouse.move(...to);
    await drop(page, frame);
    await page.keyboard.up('Alt');
    near(box(await read(page), 'm'), free);
    await page.close();
  });

  await t.test('a peer sees Alt release the snap without another pointer sample', async () => {
    const content = [odd, rect('m', 100, 600, 20, 50), rect('z', 700, 150, 30, 30)];
    const mover = await open({content, viewport:{width:1400, height:900}}), peer = await open({content, viewport:{width:1400, height:900}});
    // What the peer's editor draws for m once it has the mover's latest preview.
    const peerSees = async () => {
      const preview = (await sent(mover.page, 'presence')).filter(m => m.preview).at(-1);
      await peer.page.evaluate(m => window.port.postMessage({...m, peer:42, name:'Alice'}), preview);
      await peer.frame.locator('#scene [data-shape="m"][data-live-preview="true"]').waitFor();
      return [preview.preview.shapes[0].box[0], await peer.frame.locator('#scene [data-shape="m"]').evaluate(n => n.getBBox().x)];
    };
    const previews = () => mover.page.evaluate(() => window.messages.filter(m => m.type === 'presence' && m.preview).length);
    // The left edge comes to 252, five units from the edge at 247, and snaps.
    const from = await at(mover.page, mover.frame, 110, 625);
    await mover.page.mouse.move(...from);
    await mover.page.mouse.down();
    await mover.page.mouse.move(from[0] + 100, from[1]);
    await mover.page.mouse.move(from[0] + 152, from[1]);
    await mover.page.waitForFunction(() => window.messages.findLast(m => m.type === 'presence' && m.preview)?.preview.shapes[0].box[0] === 247);
    const [snapped, drawn] = await peerSees(), count = await previews();
    assert.equal(snapped, 247);
    // Alt alone, the pointer still: the peer must learn of the free position
    // at once, not from the presence heartbeat a second and a half later.
    await mover.page.keyboard.down('Alt');
    await mover.page.waitForFunction(count => window.messages.filter(m => m.type === 'presence' && m.preview).length > count, count, {timeout:400});
    const [free, redrawn] = await peerSees();
    assert.equal(free, 252);
    near([redrawn - drawn], [5]);
    await mover.page.keyboard.up('Alt');
    await mover.page.mouse.up();
    await mover.page.close();
    await peer.page.close();
  });

  await t.test('equal spacing continues a row and shows the gap', async () => {
    const {page, frame} = await open({content:[rect('a', 100, 120, 160, 80), rect('b', 320, 120, 160, 80), rect('c', 580, 163, 160, 80)]});
    await dragTo(page, frame, [660, 203], [617, 163]);
    assert.deepEqual(await frame.locator('#selection .guides text').allTextContents(), ['60', '60']);
    await drop(page, frame);
    assert.deepEqual(box(await read(page), 'c'), [540, 120, 160, 80]);
    await page.close();
  });

  await t.test('the centre of the visible canvas is a guide', async () => {
    const {page, frame} = await open({content:[rect('m', 100, 100, 200, 40), rect('z', 700, 500, 50, 50)]});
    const centre = await frame.locator('#canvas').evaluate(canvas => {
      const r = canvas.getBoundingClientRect();
      return new DOMPoint((r.left + r.right) / 2, 0).matrixTransform(canvas.getScreenCTM().inverse()).x;
    });
    assert.ok(Math.abs(centre - 200) > 20, 'the object starts away from the centre');
    await dragTo(page, frame, [200, 120], [centre + 3, 320]);
    assert.deepEqual(await frame.locator('#selection .guides text').allTextContents(), ['Centre of your view']);
    assert.equal(await frame.locator('#selection .guides .line.dash').count(), 1);
    await drop(page, frame);
    near([box(await read(page), 'm')[0] + 100], [centre]);
    await page.close();
  });

  await t.test('a multiple selection snaps as one and keeps its spacing', async () => {
    const {page, frame} = await open({content:[rect('p', 103, 107, 60, 40), rect('q', 191, 163, 30, 30)], boardView:{snap:true, guides:false}});
    await page.mouse.click(...await at(page, frame, 133, 127));
    await page.keyboard.down('Shift');
    await page.mouse.click(...await at(page, frame, 206, 178));
    await page.keyboard.up('Shift');
    await dragTo(page, frame, [206, 178], [236, 198]);
    await drop(page, frame);
    const content = await read(page);
    // The pair's top-left corner goes from (103, 107) to (140, 120); q follows.
    assert.deepEqual(box(content, 'p'), [140, 120, 60, 40]);
    assert.deepEqual(box(content, 'q'), [228, 176, 30, 30]);
    await page.close();
  });

  await t.test('an object nobody can see does not pull', async () => {
    const drag = async (content) => {
      const {page, frame} = await open({content:[...content, rect('m', 100, 600, 20, 50), rect('z', 700, 150, 30, 30)]});
      await dragTo(page, frame, [110, 625], [261, 628]);
      const lines = await frame.locator('#selection .guides .line').count();
      await drop(page, frame);
      const x = box(await read(page), 'm')[0];
      await page.close();
      return [Math.round(x), lines > 0];
    };
    // The left edge comes to 251, four units from the target's edge at 247.
    assert.deepEqual(await drag([odd]), [247, true]);
    assert.deepEqual(await drag([{...odd, opacity:0}]), [251, false]);
    const hidden = {id:'f', type:'frame', x:200, y:350, width:300, height:200, opacity:0};
    assert.deepEqual(await drag([hidden, {...odd, frameId:'f'}]), [251, false]);
    assert.deepEqual(await drag([{...hidden, opacity:100}, {...odd, frameId:'f'}]), [247, true]);
    // The same child placed outside its frame is clipped away entirely.
    assert.deepEqual(await drag([{...hidden, opacity:100}, {...odd, y:100, frameId:'f'}]), [251, false]);
  });

  await t.test('a tie between two targets does not depend on their layers', async () => {
    const low = rect('a1', 100, 300, 50, 50), high = rect('a2', 106, 400, 50, 50), far = rect('z', 700, 150, 30, 30);
    for (const order of [['a1', 'a2'], ['a2', 'a1']]) {
      // The same two targets, stacked one way and then the other.
      const content = [low, high].map(e => ({...e, index:e.id === order[0] ? 'a1' : 'a2'}));
      // A window the board fits at 100%, so pixels are board units and the tie is exact.
      const {page, frame} = await open({content:[...content, {...rect('m', 53, 600, 20, 20), index:'a3'}, {...far, index:'a4'}],
        viewport:{width:1400, height:900}});
      assert.deepEqual(await frame.locator('#scene [data-shape]').evaluateAll(all => all.map(n => n.dataset.shape).slice(0, 2)), order);
      assert.equal(await frame.locator('#canvas').evaluate(c => c.getScreenCTM().a), 1);
      // Fifty pixels right, the left edge is three units from both targets' edges.
      const from = await at(page, frame, 63, 610);
      await page.mouse.move(...from);
      await page.mouse.down();
      await page.mouse.move(from[0] + 25, from[1]);
      await page.mouse.move(from[0] + 50, from[1]);
      await drop(page, frame);
      assert.equal(box(await read(page), 'm')[0], 100, `layer order ${order}`);
      await page.close();
    }
  });

  await t.test('a group keeps every coordinate the move does not change', async () => {
    const p = rect('p', 100.123456789, 57.987654321, 60, 40), q = rect('q', 191.3333333333, 163.1111111111, 30, 30);
    const {page, frame} = await open({content:[p, q, rect('z', 700, 500, 50, 50)]});
    await page.mouse.click(...await at(page, frame, 130, 78));
    await page.keyboard.down('Shift');
    await page.mouse.click(...await at(page, frame, 206, 178));
    await page.keyboard.up('Shift');
    // Straight to the right: nothing snaps, and no y may change at all.
    const from = await at(page, frame, 206, 178), scale = await frame.locator('#canvas').evaluate(c => c.getScreenCTM().a);
    await page.mouse.move(...from);
    await page.mouse.down();
    await page.mouse.move(from[0] + 20, from[1]);
    await page.mouse.move(from[0] + 37, from[1]);
    await drop(page, frame);
    const content = await read(page), moved = [box(content, 'p'), box(content, 'q')];
    assert.deepEqual(moved.map(b => b[1]), [p.y, q.y]);
    near([moved[0][0] - p.x, moved[1][0] - q.x], [37 / scale, 37 / scale], 1e-9);
    await page.close();
  });

  await t.test('a view-only board offers the grid alone', async () => {
    const {page, frame} = await open({content:[rect('m', 103, 107, 90, 50)], readOnly:true});
    assert.deepEqual(await frame.locator('#view-menu input').evaluateAll(all => all.map(i => i.id)), ['view-grid']);
    await page.mouse.click(...await at(page, frame, 400, 300));
    await page.keyboard.press('Shift+G');
    await page.keyboard.press('s');
    await page.keyboard.press('g');
    assert.deepEqual(await views(page, 1), [{grid:false, snap:false, guides:true}]);
    await page.close();
  });
});

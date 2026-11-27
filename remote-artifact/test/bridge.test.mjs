import test from 'node:test';
import assert from 'node:assert/strict';
import { PassThrough } from 'node:stream';
import { bridge } from '../src/bridge.mjs';
import { startFakeRoom } from './fake-room.mjs';

const sleep = (ms) => new Promise((resolve) => setTimeout(resolve, ms));

/* A running bridge: send(message) writes one request line; next() resolves with the next output line. */
function startBridge(t, options = {}) {
  const input = new PassThrough();
  const output = new PassThrough();
  const lines = [];
  const waiting = [];
  let raw = '';
  output.on('data', (chunk) => {
    raw += chunk;
    let end;
    while ((end = raw.indexOf('\n')) >= 0) {
      const message = JSON.parse(raw.slice(0, end));
      raw = raw.slice(end + 1);
      if (waiting.length) waiting.shift()(message); else lines.push(message);
    }
  });
  const done = bridge({ input, output, ...options });
  t.after(async () => { input.end(); await done; });
  return {
    send: (message) => input.write(`${JSON.stringify(message)}\n`),
    next: () => (lines.length ? Promise.resolve(lines.shift()) : new Promise((resolve) => waiting.push(resolve))),
    get output() { return raw; },
    input,
    done,
  };
}

test('a request without body answers the welcome; one with a body the origin reply', async (t) => {
  const origin = await startFakeRoom();
  t.after(() => origin.close());
  const b = startBridge(t);
  const link = origin.link('edit');
  b.send({ id: 1, link, guestId: 'g', name: 'tester@host' });
  const probe = await b.next();
  assert.deepEqual(probe, { id: 1, welcome: { id: 'snt-pipeline', kind: 'whiteboard',
    title: 'SNT analysis pipeline', readOnly: false } });
  b.send({ id: 2, link, guestId: 'g', name: 'tester@host', body: { action: 'read' } });
  const read = await b.next();
  assert.equal(read.id, 2);
  assert.match(read.reply.result.text, /SNT analysis pipeline/);
  // The origin id is pinned by the bridge, whatever the request said.
  assert.equal(origin.received.find((f) => f.t === 'editing').body.id, 'snt-pipeline');
  assert.equal(origin.connections, 1);
});

test('a probe answers within its deadline and leaves no connection or watch', async (t) => {
  const origin = await startFakeRoom();
  t.after(() => origin.close());
  const old = await startFakeRoom({ mode: 'old' });
  t.after(() => old.close());
  const b = startBridge(t);
  const link = origin.link('edit');
  // Its body is not sent: a probe only reads the welcome.
  b.send({ id: 1, link, guestId: 'g', name: 'n', ref: 'R1', probeMs: 2000, body: { action: 'read' } });
  assert.deepEqual(await b.next(), { id: 1, welcome: { id: 'snt-pipeline', kind: 'whiteboard',
    title: 'SNT analysis pipeline', readOnly: false } });
  await sleep(50);
  assert.equal(origin.open, 0);
  assert.ok(!origin.received.some((f) => f.t === 'editing'));
  // A silent origin is diagnosed at the probe's deadline, not the default 10 s.
  const start = Date.now();
  b.send({ id: 2, link: old.link('edit'), guestId: 'g', name: 'n', probeMs: 200 });
  assert.deepEqual(await b.next(), { id: 2, error: 'Origin may run an older protocol; update niwa on the host' });
  assert.ok(Date.now() - start < 2000);
  await sleep(50);
  assert.equal(old.open, 0);
  // Other users are undisturbed: a pooled request connects and only it is watched.
  b.send({ id: 3, link, guestId: 'g', name: 'n', ref: 'R3' });
  assert.equal((await b.next()).id, 3);
  assert.equal(origin.open, 1);
  await origin.event({ event: 'changed', id: 'snt-pipeline', kind: 'whiteboard', title: 'Renamed', revision: 4 });
  assert.equal((await b.next()).ref, 'R3');
});

test('each display name has its own connection, so edits carry it', async (t) => {
  const origin = await startFakeRoom();
  t.after(() => origin.close());
  const b = startBridge(t);
  const link = origin.link('edit');
  b.send({ id: 1, link, guestId: 'g', name: 'tester@host' });
  await b.next();
  b.send({ id: 2, link, guestId: 'g', name: 'tester@host /root/worker' });
  await b.next();
  b.send({ id: 3, link, guestId: 'g', name: 'tester@host' });
  await b.next();
  assert.equal(origin.connections, 2);
  assert.deepEqual(origin.received.filter((f) => f.t === 'hello').map((f) => f.name),
    ['tester@host', 'tester@host /root/worker']);
});

test('connections open lazily and close when idle', async (t) => {
  const origin = await startFakeRoom();
  t.after(() => origin.close());
  const b = startBridge(t, { idleMs: 50 });
  await sleep(20);
  assert.equal(origin.connections, 0);
  b.send({ id: 1, link: origin.link('view'), guestId: 'g', name: 'n' });
  await b.next();
  await sleep(200);
  b.send({ id: 2, link: origin.link('view'), guestId: 'g', name: 'n' });
  await b.next();
  assert.equal(origin.connections, 2);
});

test('failures answer with their message and the link is never written', async (t) => {
  const absent = await startFakeRoom({ mode: 'absent' });
  t.after(() => absent.close());
  const session = await startFakeRoom({ mode: 'session' });
  t.after(() => session.close());
  const b = startBridge(t);
  b.send({ id: 1, link: absent.link('edit'), guestId: 'g', name: 'n', body: { action: 'read' } });
  assert.deepEqual(await b.next(), { id: 1, error: 'Origin unavailable (revoked, deleted, or offline)' });
  b.send({ id: 2, link: session.link('edit'), guestId: 'g', name: 'n' });
  assert.match((await b.next()).error, /not an artifact link/);
  b.send({ id: 3, link: 'not a link', guestId: 'g', name: 'n' });
  assert.equal((await b.next()).id, 3);
  b.send('garbage');
  assert.equal((await b.next()).id, null);
  assert.ok(!b.output.includes(absent.roomId));
});

test('origin change events are passed on with the request ref', async (t) => {
  const origin = await startFakeRoom();
  t.after(() => origin.close());
  const b = startBridge(t);
  b.send({ id: 1, link: origin.link('view'), guestId: 'g', name: 'n', ref: 'R1' });
  await b.next();
  await origin.event({ event: 'changed', id: 'snt-pipeline', kind: 'whiteboard', title: 'Renamed', revision: 4 });
  assert.deepEqual(await b.next(), { ref: 'R1', event: { event: 'changed', id: 'snt-pipeline',
    kind: 'whiteboard', title: 'Renamed', revision: 4 } });
});

test('the bridge ends with its input and closes its connections', async (t) => {
  const origin = await startFakeRoom();
  t.after(() => origin.close());
  const b = startBridge(t);
  b.send({ id: 1, link: origin.link('view'), guestId: 'g', name: 'n' });
  await b.next();
  b.input.end();
  await b.done;
});

import assert from 'node:assert/strict';
import test from 'node:test';
import { generateKeyBetween } from 'fractional-indexing';
import * as Y from 'yjs';
import { compareOrder, create, fieldValid, repairLegacyPositions } from '../model.mjs';

test('element positions accept generated keys and null', () => {
  const first = generateKeyBetween(null, null);
  const next = generateKeyBetween(first, null);
  for (const index of [null, first, next, generateKeyBetween(null, first),
    generateKeyBetween(first, next), 'a' + '1'.repeat(127)])
    assert.equal(fieldValid('index', index), true);
});

test('element positions require bounded, well-formed order keys', () => {
  for (const index of ['b1', '00', 'a0V0', 'a!', 'a' + '1'.repeat(128), 'z'.repeat(128), 1])
    assert.equal(fieldValid('index', index), false, `index: ${index}`);
});

test('legacy repair preserves draw order, including ties and unpositioned elements', () => {
  for (const [upper, tight] of [['a1', false], ['a0' + '0'.repeat(125) + '1', true]]) {
    const doc = create('whiteboard', 'Order');
    try {
      const elements = doc.getMap('elements');
      for (const [id, index] of [['u', null], ['z', 'a00'], ['y', 'a00'], ['l', 'a0'], ['r', upper]])
        elements.set(id, new Y.Map([['index', index]]));
      const order = () => [...elements].map(([id, e]) => ({ id, index: e.get('index') }))
        .sort(compareOrder).map((e) => e.id);
      const before = order();
      assert.equal(repairLegacyPositions(doc), 2);
      assert.deepEqual(order(), before);
      assert.equal(elements.get('u').get('index'), null);
      assert.equal(elements.get('r').get('index') !== upper, tight, 'only exhausted gaps require rekeying neighbours');
      assert.equal(repairLegacyPositions(doc), 0);
    } finally { doc.destroy(); }
  }
});

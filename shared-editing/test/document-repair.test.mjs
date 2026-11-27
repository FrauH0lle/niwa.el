import test from 'node:test';
import assert from 'node:assert/strict';
import * as Y from 'yjs';
import { repairLegacyDocumentAttrs } from '../document.mjs';

test('legacy repair preserves nested node and text identities and unrelated formatting', () => {
  const doc = new Y.Doc(), root = doc.getXmlFragment('document');
  const quote = new Y.XmlElement('blockquote'), block = new Y.XmlElement('paragraph'), text = new Y.XmlText();
  root.insert(0, [quote]); quote.insert(0, [block]); block.insert(0, [text]);
  block.setAttribute('id', 'paragraph');
  block.setAttribute('toString', 'legacy');
  block.setAttribute('unrelated', 'leave');
  const link = 'link--abcdefgh', attrs = { href: '#keep', unrelated: 'leave' };
  text.insert(0, 'A🙂');
  text.insert(3, 'B', { [link]: { ...attrs, constructor: 'legacy' }, bold: {} });
  const anchor = Y.createRelativePositionFromTypeIndex(text, 3);

  assert.equal(repairLegacyDocumentAttrs(doc), 2);
  assert.equal(root.get(0).get(0), block);
  assert.deepEqual(block.getAttributes(), { id: 'paragraph', unrelated: 'leave' });
  const absolute = Y.createAbsolutePositionFromRelativePosition(anchor, doc);
  assert.equal(absolute.type, text);
  assert.equal(absolute.index, 3);
  assert.deepEqual(text.toDelta(), [{ insert: 'A🙂' }, { insert: 'B', attributes: { [link]: attrs, bold: {} } }]);
  assert.equal(repairLegacyDocumentAttrs(doc), 0);
});

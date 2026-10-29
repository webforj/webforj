// Run with: node --test src/test/js/matchmedia.test.cjs
// Execute the shipped bridge resource, without introducing a frontend build dependency.
const assert = require('node:assert/strict');
const { readFileSync } = require('node:fs');
const path = require('node:path');
const { test } = require('node:test');
const vm = require('node:vm');

const script = readFileSync(path.resolve(__dirname,
  '../../main/resources/static/webforj/matchmedia/matchmedia.js'), 'utf8');

function browser() {
  const queries = [];
  const events = [];
  const document = new EventTarget();
  document.body = document;
  document.addEventListener('webforj-media-query-change', event => events.push(event.detail));
  const window = {
    matchMedia(query) {
      const target = new EventTarget();
      const handlers = new Set();
      const media = {
        media: query.trim(),
        matches: false,
        handlers,
        addEventListener(type, handler) {
          handlers.add(handler);
          target.addEventListener(type, handler);
        },
        removeEventListener(type, handler) {
          handlers.delete(handler);
          target.removeEventListener(type, handler);
        },
        change(matches) {
          if (matches === this.matches) return;
          this.matches = matches;
          const event = new Event('change');
          event.media = this.media;
          event.matches = matches;
          target.dispatchEvent(event);
        }
      };
      queries.push(media);
      return media;
    }
  };
  const context = vm.createContext({ window, document, CustomEvent });
  vm.runInContext(script, context);
  return {
    window, queries, events, context,
    call(command, values = {}) {
      return JSON.parse(window.__webforjMatchMedia.call({ command, ...values }));
    }
  };
}

test('one native handler serves multiple registrations and the last removal detaches it', () => {
  const b = browser();
  b.call('create', { id: 'q', query: '(max-width: 700px)' });
  b.call('subscribe', { id: 'q', listener: 'a', initial: false });
  b.call('subscribe', { id: 'q', listener: 'b', initial: false });
  assert.equal(b.queries[0].handlers.size, 1);
  assert.equal(b.events.length, 0);
  b.queries[0].change(true);
  assert.deepEqual(Array.from(b.events[0].listeners), ['a', 'b']);
  assert.equal(b.events[0].initial, false);
  b.call('unsubscribe', { id: 'q', listener: 'a' });
  assert.equal(b.queries[0].handlers.size, 1);
  b.call('unsubscribe', { id: 'q', listener: 'b' });
  assert.equal(b.queries[0].handlers.size, 0);
  b.queries[0].change(false);
  assert.equal(b.events.length, 1);
  assert.equal(b.call('matches', { id: 'q' }).value, false);
});

test('initial notification is targeted, preserves false, and uses serialized media', () => {
  const b = browser();
  b.call('create', { id: 'q', query: '  (width >= 700px)  ' });
  b.call('subscribe', { id: 'q', listener: 'a' });
  b.call('subscribe', { id: 'q', listener: 'b', initial: true });
  assert.equal(b.events.length, 1);
  assert.deepEqual(Array.from(b.events[0].listeners), ['b']);
  assert.equal(b.events[0].media, '(width >= 700px)');
  assert.equal(b.events[0].matches, false);
  assert.equal(b.events[0].initial, true);
  b.queries[0].change(true);
  assert.deepEqual(Array.from(b.events[1].listeners), ['a', 'b']);
  assert.equal(b.events[1].initial, false);
});

test('queued events retain the registrations that existed when the change happened', () => {
  const b = browser();
  b.call('create', { id: 'q', query: 'screen' });
  b.call('subscribe', { id: 'q', listener: 'old' });
  b.queries[0].change(true);
  b.call('subscribe', { id: 'q', listener: 'new' });
  assert.deepEqual(Array.from(b.events[0].listeners), ['old']);
});

test('removing and adding listeners again does not accumulate native handlers', () => {
  const b = browser();
  b.call('create', { id: 'q', query: 'screen' });
  b.call('subscribe', { id: 'q', listener: 'old' });
  b.call('unsubscribe', { id: 'q', listener: 'old' });
  b.call('subscribe', { id: 'q', listener: 'new', initial: true });
  b.call('unsubscribe', { id: 'q', listener: 'old' });
  assert.equal(b.queries[0].handlers.size, 1);
  b.queries[0].change(true);
  assert.equal(b.events.length, 2);
  assert.deepEqual(Array.from(b.events[1].listeners), ['new']);
});

test('queries with identical strings have independent ownership and cleanup', () => {
  const b = browser();
  for (const id of ['first', 'second']) {
    b.call('create', { id, query: 'screen' });
    b.call('subscribe', { id, listener: id });
  }
  b.call('destroy', { id: 'first' });
  b.call('destroy', { id: 'first' });
  assert.equal(b.queries[0].handlers.size, 0);
  assert.equal(b.queries[1].handlers.size, 1);
  b.queries[0].change(true);
  b.queries[1].change(true);
  assert.equal(b.events.length, 1);
  assert.equal(b.events[0].id, 'second');
});

test('bulk teardown removes only its owned queries and allows later initialization', () => {
  const b = browser();
  for (const id of ['old', 'new']) {
    b.call('create', { id, query: 'screen' });
    b.call('subscribe', { id, listener: id });
  }
  b.call('destroyAll', { ids: ['old'] });
  assert.equal(b.queries[0].handlers.size, 0);
  assert.equal(b.queries[1].handlers.size, 1);
  const bridge = b.window.__webforjMatchMedia;
  vm.runInContext(script, b.context);
  assert.equal(b.window.__webforjMatchMedia, bridge);
  assert.equal(b.call('matches', { id: 'new' }).ok, true);
  assert.equal(b.call('matches', { id: 'old' }).ok, false);
});

test('unsupported APIs and invalid commands return errors instead of match results', () => {
  const b = browser();
  delete b.window.matchMedia;
  assert.equal(b.call('create', { id: 'q', query: 'screen' }).ok, false);
  assert.equal(b.call('subscribe', { id: 'missing', listener: 'a' }).ok, false);
  assert.equal(b.call('invalid').ok, false);
});

test('duplicate query IDs cannot overwrite a live query and leak its listener', () => {
  const b = browser();
  b.call('create', { id: 'q', query: 'screen' });
  b.call('subscribe', { id: 'q', listener: 'a' });
  assert.equal(b.call('create', { id: 'q', query: 'print' }).ok, false);
  assert.equal(b.queries.length, 1);
  b.call('destroy', { id: 'q' });
  assert.equal(b.queries[0].handlers.size, 0);
});

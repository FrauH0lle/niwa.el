/* collaboration-notifications-test.js -- browser notification assertions
 *
 * Run directly with: node test/collaboration-notifications-test.js
 * The full viewer runner also includes these checks.
 */
'use strict';

const assert = require('node:assert/strict');
const fs = require('node:fs');
const vm = require('node:vm');

class Button {
  constructor() {
    this.listeners = {};
    this.attributes = {};
    this.hidden = false;
  }
  addEventListener(type, callback) { this.listeners[type] = callback; }
  setAttribute(name, value) { this.attributes[name] = value; }
  dispatch(type) { this.listeners[type](); }
}

const tick = () => new Promise(resolve => setImmediate(resolve));

async function waitFor(predicate, what) {
  for (let i = 0; i < 500; i++) {
    if (predicate()) return;
    await tick();
  }
  throw new Error(`timed out waiting for ${what}`);
}

async function testServiceWorker() {
  const listeners = {};
  const notifications = [];
  let opened;
  const share = 'roomroomroomroom.secret';
  const client = {
    url: 'https://relay.example/', visibilityState: 'hidden', focused: false,
    focus: async () => { client.focused = true; },
  };
  const self = {
    addEventListener(type, callback) { listeners[type] = callback; },
    registration: {
      scope: `https://relay.example/pwa/${share}/`,
      showNotification: async (title, options) => notifications.push({title, options}),
    },
    location: {origin: 'https://relay.example'},
    clients: {
      matchAll: async () => [client],
      openWindow: async url => { opened = url; },
    },
  };
  vm.runInNewContext(fs.readFileSync('relay/viewer/service-worker.js', 'utf8'),
                     {self, URL});
  let pending;
  listeners.push({waitUntil(promise) { pending = promise; }});
  await pending;
  assert.equal(notifications.length, 1);
  assert.equal(notifications[0].title, 'New niwa activity');
  assert.doesNotMatch(JSON.stringify(notifications[0]), /prompt|transcript|secret/i);

  let closed = false;
  listeners.notificationclick({
    notification: {close() { closed = true; }},
    waitUntil(promise) { pending = promise; },
  });
  await pending;
  assert.equal(closed, true);
  assert.equal(client.focused, false);
  assert.equal(opened, `https://relay.example/#${share}`);
}

async function testNotificationsModule() {
  const storage = new Map();
  const tabStorage = new Map();
  const registrations = new Map();
  const unsubscribed = [];
  const unregistered = [];
  const sent = [];
  const makeRegistration = scope => {
    let subscription = null;
    return {
      scope,
      unregister: async () => {
        unregistered.push(scope);
        registrations.delete(scope);
        return true;
      },
      pushManager: {
        getSubscription: async () => subscription,
        subscribe: async options => {
          subscription = {
            endpoint: `https://push.example/${registrations.size}`,
            options,
            unsubscribe: async () => {
              unsubscribed.push(scope);
              subscription = null;
            },
          };
          return subscription;
        },
      },
    };
  };
  const window = {
    location: {hash: ''},
    prompt: () => 'https://relay.example/#freshfreshfresh1.fresh-secret',
    matchMedia: () => ({matches: true}),
    addEventListener() {},
  };
  const context = {
    window,
    document: {title: 'niwa', hidden: false, hasFocus: () => true,
               addEventListener() {}},
    navigator: {
      standalone: true,
      serviceWorker: {
        register: async (script, options) => {
          assert.equal(script, '/service-worker.js');
          assert.doesNotMatch(script, /fresh-secret|room-secret/);
          if (!registrations.has(options.scope)) {
            registrations.set(options.scope, makeRegistration(options.scope));
          }
          return registrations.get(options.scope);
        },
        getRegistration: async scope => registrations.get(scope),
      },
    },
    Notification: class Notification {
      static permission = 'granted';
      static async requestPermission() { return 'granted'; }
    },
    PushManager: class PushManager {},
    localStorage: {
      getItem: key => storage.get(key) || null,
      setItem: (key, value) => storage.set(key, value),
      removeItem: key => storage.delete(key),
    },
    sessionStorage: {
      getItem: key => tabStorage.get(key) || null,
      setItem: (key, value) => tabStorage.set(key, value),
      removeItem: key => tabStorage.delete(key),
    },
    fetch: async () => ({ok: true, json: async () => ({key: 'push-key'})}),
    Uint8Array, URL, encodeURIComponent, decodeURIComponent,
  };
  vm.runInNewContext(fs.readFileSync('relay/viewer/notifications.js', 'utf8'), context);
  const api = window.niwaViewerNotifications;
  const parse = fragment => /^#[^.]{16}\..+/.test(fragment) ? {} : null;
  storage.set('niwa-last-share', 'storedstoredsto1.stored-secret');
  tabStorage.set('niwa-tab-share', 'tabtabtabtabtab1.tab-secret');
  assert.equal(api.resolveShare(parse), 'tabtabtabtabtab1.tab-secret');
  tabStorage.clear();
  assert.equal(api.resolveShare(parse), 'storedstoredsto1.stored-secret');
  storage.clear();
  assert.equal(api.resolveShare(parse), 'freshfreshfresh1.fresh-secret');

  const decode = () => Uint8Array.from({length: 65}, (_, index) => index);
  const make = (fragment, write = async frame => sent.push({fragment, frame})) => {
    const button = new Button();
    const state = {fragment, connected: true, writable: true, pushSubscribed: false};
    const flashed = [];
    const notifications = api.create({
      state,
      button, decode, flash: message => flashed.push(message),
      send: write,
    });
    notifications.bind();
    return {button, notifications, state, flashed};
  };

  // A session view link keeps in-tab notifications but never subscribes to
  // background push; the bell says background delivery needs an edit link.
  const pushNeedsEdit = 'Background notifications need an edit link. Ask the host for one.';
  const viewFragment = 'viewonlyroomroo1.view-secret';
  const view = make(viewFragment);
  view.state.writable = false;
  view.notifications.render();
  assert.equal(view.button.hidden, false);
  assert.match(view.button.attributes['aria-label'], /while this tab is away/);
  view.button.dispatch('click');
  await waitFor(() => view.notifications.enabled(), 'view link in-tab opt-in');
  await waitFor(() => view.flashed.length === 1, 'view link explanation');
  assert.equal(view.flashed[0], `Notifications on while this tab is open. ${pushNeedsEdit}`);
  assert.equal(await view.notifications.syncPush(), false);
  assert.equal(view.state.pushSubscribed, false);
  assert.equal(registrations.size, 0);
  assert.equal(sent.some(item => item.frame.t === 'push-subscribe'), false);
  view.button.dispatch('click');
  await waitFor(() => !view.notifications.enabled(), 'view link opt-out');
  await waitFor(() => sent.length === 1, 'view link opt-out send');
  sent.length = 0;
  storage.clear();

  // A view-link lobby gets no workspace notices: its bell explains why and
  // asks for no browser permission.
  const viewLobby = make(viewFragment);
  viewLobby.state.writable = false;
  viewLobby.state.lobbyNotifications = true;
  storage.set(`niwa-notify:${viewFragment}`, 'on');
  viewLobby.notifications.render();
  assert.equal(viewLobby.notifications.enabled(), false);
  assert.equal(viewLobby.button.attributes['aria-label'], pushNeedsEdit);
  let asked = false;
  const request = context.Notification.requestPermission;
  context.Notification.requestPermission = async () => { asked = true; return 'granted'; };
  viewLobby.button.dispatch('click');
  await tick();
  context.Notification.requestPermission = request;
  assert.equal(asked, false);
  assert.deepEqual(viewLobby.flashed, [pushNeedsEdit]);
  assert.equal(sent.length, 0);
  storage.clear();

  const firstFragment = 'roomroomroomroom.room-secret-one';
  const secondFragment = 'otherroomroomroo.room-secret-two';
  const first = make(firstFragment);
  const second = make(secondFragment);

  first.button.dispatch('click');
  await waitFor(() => first.notifications.enabled(), 'first room opt-in');
  await waitFor(() => sent.some(item => item.fragment === firstFragment
                                       && item.frame.t === 'push-subscribe'),
                'first room push subscription');
  assert.equal(second.notifications.enabled(), false);

  second.button.dispatch('click');
  await waitFor(() => second.notifications.enabled(), 'second room opt-in');
  await waitFor(() => sent.filter(item => item.frame.t === 'push-subscribe').length === 2,
                'second room push subscription');
  assert.equal(registrations.size, 2);

  first.button.dispatch('click');
  await waitFor(() => !first.notifications.enabled(), 'first room opt-out');
  await waitFor(() => unsubscribed.length === 1, 'first room unsubscribe');
  assert.equal(second.notifications.enabled(), true);
  assert.equal(storage.has(`niwa-notify:${firstFragment}`), false);
  assert.equal(storage.get(`niwa-notify:${secondFragment}`), 'on');
  assert.deepEqual(unsubscribed, [`/pwa/${firstFragment}/`]);
  assert.deepEqual(unregistered, [`/pwa/${firstFragment}/`]);
  assert.equal(registrations.size, 1);
  assert.equal(sent.filter(item => item.frame.t === 'push-unsubscribe').length, 1);

  second.notifications.forget();
  await waitFor(() => unregistered.length === 2, 'second room unregister');
  assert.equal(storage.has(`niwa-notify:${secondFragment}`), false);
  assert.equal(storage.has('niwa-last-share'), false);
  assert.equal(registrations.size, 0);

  const failedFragment = 'failedroomroom12.room-secret-four';
  storage.set(`niwa-notify:${failedFragment}`, 'on');
  const failed = make(failedFragment, async frame => {
    sent.push({fragment: failedFragment, frame});
    return false;
  });
  assert.equal(await failed.notifications.syncPush(), false);
  assert.equal(failed.state.pushSubscribed, false);
  await failed.notifications.dropPush();
  assert.equal(registrations.size, 0);

  // A teardown send failure remains observable, but a queued setup still runs.
  const retryFragment = 'retryroomroom123.room-secret-six';
  storage.set(`niwa-notify:${retryFragment}`, 'on');
  const retry = make(retryFragment, async frame => {
    if (frame.t === 'push-unsubscribe') throw new Error('send failed');
    sent.push({fragment: retryFragment, frame});
    return true;
  });
  const failedDrop = assert.rejects(retry.notifications.dropPush(), /send failed/);
  const recoveredSync = retry.notifications.syncPush();
  await failedDrop;
  assert.equal(await recoveredSync, true);
  assert.equal(retry.state.pushSubscribed, true);
  // Teardown still releases local resources when publishing opt-out fails.
  await assert.rejects(retry.notifications.dropPush(), /send failed/);
  assert.equal(registrations.size, 0);

  const terminalFragment = 'terminalroomroom.room-secret-five';
  const subscribeAtTerminal = sent.filter(item =>
    item.frame.t === 'push-subscribe').length;
  let finishPermission;
  context.Notification.requestPermission = () => new Promise(resolve => {
    finishPermission = resolve;
  });
  const terminal = make(terminalFragment);
  terminal.button.dispatch('click');
  await waitFor(() => finishPermission, 'deferred notification permission');
  terminal.state.connected = false;
  terminal.notifications.forget();
  finishPermission('granted');
  await tick();
  await tick();
  assert.equal(storage.has(`niwa-notify:${terminalFragment}`), false);
  assert.equal(storage.has('niwa-last-share'), false);
  assert.equal(sent.filter(item => item.frame.t === 'push-subscribe').length,
               subscribeAtTerminal);
  context.Notification.requestPermission = async () => 'granted';

  // Opt-out owns the lifecycle even when service-worker registration is
  // still in flight: the stale setup must not subscribe after teardown.
  const delayedFragment = 'delayedroomroom1.room-secret-three';
  const subscribeBefore = sent.filter(item =>
    item.frame.t === 'push-subscribe').length;
  let releaseRegister;
  context.navigator.serviceWorker.register = async (_script, options) => {
    const delayed = makeRegistration(options.scope);
    registrations.set(options.scope, delayed);
    return new Promise(resolve => { releaseRegister = () => resolve(delayed); });
  };
  const delayed = make(delayedFragment);
  delayed.button.dispatch('click');
  await waitFor(() => releaseRegister, 'deferred service-worker registration');
  delayed.button.dispatch('click');
  releaseRegister();
  await waitFor(() => !registrations.size, 'deferred registration teardown');
  assert.equal(delayed.notifications.enabled(), false);
  assert.equal(sent.filter(item => item.frame.t === 'push-subscribe').length,
               subscribeBefore);
}

async function testLivePageFallback() {
  const fragment = 'roomroomroomroom.room-secret';
  const storage = new Map([[`niwa-notify:${fragment}`, 'on']]);
  const shown = [];
  const document = {
    title: 'niwa', hidden: false, hasFocus: () => !document.hidden,
    addEventListener() {},
  };
  const window = {addEventListener() {}};
  const context = {
    window, document, navigator: {},
    Notification: class Notification {
      static permission = 'granted';
      constructor(title, options) { shown.push({title, options}); }
    },
    localStorage: {
      getItem: key => storage.get(key) || null,
      setItem: (key, value) => storage.set(key, value),
      removeItem: key => storage.delete(key),
    },
    Uint8Array, encodeURIComponent,
  };
  vm.runInNewContext(fs.readFileSync('relay/viewer/notifications.js', 'utf8'), context);
  const state = {fragment, connected: true, writable: true, pushSubscribed: false};
  const notifications = window.niwaViewerNotifications.create({
    state,
    button: null, decode() {}, flash() {}, send: async () => {},
  });

  notifications.maybeNotify('Focused', 'ignored');
  assert.equal(shown.length, 0);
  document.hidden = true;
  notifications.maybeNotify('Away', 'shown');
  assert.equal(shown.length, 1);
  assert.equal(shown[0].title, 'Away');
  assert.equal(shown[0].options.body, 'shown');
  // A session view link keeps in-tab notifications.
  state.writable = false;
  notifications.maybeNotify('View link', 'shown');
  assert.equal(shown.length, 2);
  assert.equal(shown[1].title, 'View link');
  state.writable = true;
  storage.set(`niwa-notify:${fragment}`, 'off');
  notifications.maybeNotify('Opted out', 'ignored');
  assert.equal(shown.length, 2);
}

async function runNotificationTests() {
  await testServiceWorker();
  await testNotificationsModule();
  await testLivePageFallback();
}

if (require.main === module) {
  runNotificationTests().then(() => console.log('viewer notifications passed'))
    .catch(error => {
      console.error(error);
      process.exit(1);
    });
}

module.exports = runNotificationTests;

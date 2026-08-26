'use strict';

const NAVIGATE_MESSAGE = 'webforj-push-navigate';
const params = new URL(self.location.href).searchParams;
const withSlash = (value) => (value.endsWith('/') ? value : `${value}/`);
const root = new URL(withSlash(params.get('root') || '/'), self.location.origin);
const assets = new URL('../../', self.location.href);
const icons = params.get('icons') ? new URL(params.get('icons'), self.location.origin) : null;

const join = (base, path) => new URL(path.replace(/^\/+/, ''), withSlash(base.href)).href;

const PROTOCOLS = [
  { prefix: 'icons://', base: () => icons },
  { prefix: 'webserver://', base: () => assets },
  { prefix: 'ws://', base: () => assets }
];

const resolve = (url) => {
  if (!url) {
    return undefined;
  }

  const lower = url.toLowerCase();
  const protocol = PROTOCOLS.find(({ prefix }) => lower.startsWith(prefix));
  if (!protocol) {
    return new URL(url, root).href;
  }

  const base = protocol.base();
  return base ? join(base, url.substring(protocol.prefix.length)) : undefined;
};

const OPTION_FIELDS = {
  body: (value) => value,
  tag: (value) => value,
  icon: resolve,
  silent: Boolean
};

const toOptions = (message) => {
  const actions = message.actions || [];
  const options = Object.fromEntries(Object.entries(OPTION_FIELDS)
    .filter(([key]) => message[key])
    .map(([key, convert]) => [key, convert(message[key])]));

  if (actions.length) {
    options.actions = actions.map(({ action, title }) => ({ action, title }));
  }

  options.data = {
    url: resolve(message.url) || root.href,
    actions: Object.fromEntries(actions.map(({ action, url }) => [action, resolve(url)]))
  };

  return options;
};

const open = async (target) => {
  const windows = await self.clients.matchAll({ type: 'window', includeUncontrolled: true });

  const exact = windows.find((client) => client.url === target);
  if (exact) {
    return exact.focus();
  }

  const same = windows.find((client) => client.url === root.href.slice(0, -1)
    || client.url.startsWith(root.href));
  if (same) {
    same.postMessage({ type: NAVIGATE_MESSAGE, url: target });
    return same.focus();
  }

  return self.clients.openWindow(target);
};

self.addEventListener('install', () => {
  self.skipWaiting();
});

self.addEventListener('push', (event) => {
  let message = null;
  try {
    message = event.data ? event.data.json() : null;
  } catch (e) {
    message = null;
  }

  if (!message?.title) {
    return;
  }

  event.waitUntil(self.registration.showNotification(message.title, toOptions(message)));
});

self.addEventListener('notificationclick', (event) => {
  event.notification.close();

  const data = event.notification.data || {};
  const target = (event.action && data.actions?.[event.action]) || data.url || root.href;
  event.waitUntil(open(target));
});

self.addEventListener('pushsubscriptionchange', (event) => {
  const previous = event.oldSubscription;
  if (!previous?.options) {
    return;
  }

  event.waitUntil(self.registration.pushManager.subscribe(previous.options));
});

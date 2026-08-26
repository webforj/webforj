(() => {
  'use strict';

  const NAVIGATE_MESSAGE = 'webforj-push-navigate';
  const queued = (window.__webforjPush && window.__webforjPush._q) || [];
  const registrations = {};

  const failure = (code, message) => {
    const error = new Error(message);
    error.code = code;
    return error;
  };

  const isSupported = () => !!(window.isSecureContext && 'serviceWorker' in navigator
    && 'PushManager' in window && 'Notification' in window);

  const toApplicationServerKey = (key) => {
    const padding = '='.repeat((4 - (key.length % 4)) % 4);
    const base64 = (key + padding).replace(/-/g, '+').replace(/_/g, '/');
    const raw = window.atob(base64);
    return Uint8Array.from(raw, (char) => char.charCodeAt(0));
  };

  const isSignedWith = (subscription, applicationServerKey) => {
    const current = subscription.options && subscription.options.applicationServerKey;
    if (!current) {
      return true;
    }

    const bytes = new Uint8Array(current);
    return bytes.length === applicationServerKey.length
      && bytes.every((byte, index) => byte === applicationServerKey[index]);
  };

  const serialize = (subscription) => {
    const json = subscription.toJSON();
    return { endpoint: json.endpoint, p256dh: json.keys.p256dh, auth: json.keys.auth };
  };

  const register = (request) => {
    if (!isSupported()) {
      return Promise.reject(failure('unsupported',
        'This browser cannot receive push notifications from this page'));
    }

    if (!registrations[request.scope]) {
      registrations[request.scope] = navigator.serviceWorker
        .register(request.worker, { scope: request.scope, updateViaCache: 'none' })
        .catch((e) => {
          delete registrations[request.scope];
          throw failure('failed', `The push worker could not be registered: ${e.message}`);
        });
    }

    return registrations[request.scope];
  };

  const whenActive = (registration) => {
    if (registration.active) {
      return Promise.resolve(registration);
    }

    const worker = registration.installing || registration.waiting;
    if (!worker) {
      return Promise.reject(failure('failed', 'The push worker did not start'));
    }

    return new Promise((resolve, reject) => {
      const onStateChange = () => {
        if (worker.state === 'activated') {
          worker.removeEventListener('statechange', onStateChange);
          resolve(registration);
        } else if (worker.state === 'redundant') {
          worker.removeEventListener('statechange', onStateChange);
          reject(failure('failed', 'The push worker was replaced before it started'));
        }
      };
      worker.addEventListener('statechange', onStateChange);
    });
  };

  const subscribe = async (request) => {
    const registration = await whenActive(await register(request));
    const permission = await Notification.requestPermission();
    if (permission !== 'granted') {
      throw failure('permission-denied', 'The user blocked notifications from this application');
    }

    const applicationServerKey = toApplicationServerKey(request.key);
    let subscription = await registration.pushManager.getSubscription();
    if (subscription && !isSignedWith(subscription, applicationServerKey)) {
      await subscription.unsubscribe();
      subscription = null;
    }

    if (!subscription) {
      subscription = await registration.pushManager.subscribe({
        userVisibleOnly: true,
        applicationServerKey
      });
    }

    return serialize(subscription);
  };

  const findSubscription = async (request) => {
    if (!isSupported()) {
      return null;
    }

    const registration = await navigator.serviceWorker.getRegistration(request.scope);
    return registration ? registration.pushManager.getSubscription() : null;
  };

  const unsubscribe = async (request) => {
    const subscription = await findSubscription(request);
    if (!subscription) {
      return null;
    }

    const serialized = serialize(subscription);
    await subscription.unsubscribe();
    return serialized;
  };

  const getSubscription = async (request) => {
    const subscription = await findSubscription(request);
    return subscription ? serialize(subscription) : null;
  };

  const getPermission = async () => ('Notification' in window ? Notification.permission : 'denied');

  const commands = {
    register: async (request) => {
      await register(request);
      return null;
    },
    subscribe,
    unsubscribe,
    getSubscription,
    getPermission
  };

  const succeed = (value) => ({ ok: true, value: value === undefined ? null : value });
  const fail = (error, message) => ({ ok: false, error, message });

  const call = (request) => {
    const command = commands[request.command];
    if (!command) {
      return Promise.resolve(fail('failed', `Unknown push command ${request.command}`));
    }

    return command(request).then(succeed, (e) => fail(e.code || 'failed', e.message || String(e)));
  };

  if (navigator.serviceWorker) {
    navigator.serviceWorker.addEventListener('message', (event) => {
      const data = event.data || {};
      if (data.type === NAVIGATE_MESSAGE && data.url && window.location.href !== data.url) {
        window.location.assign(data.url);
      }
    });
    navigator.serviceWorker.startMessages();
  }

  window.__webforjPush = { call };
  queued.forEach(({ request, resolve }) => call(request).then(resolve));
})();

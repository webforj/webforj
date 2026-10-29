(() => {
  if (window.__webforjMatchMedia) return;
  const queries = new Map();
  const emit = (id, media, matches, listeners, initial) => {
    (document.body || document).dispatchEvent(new CustomEvent('webforj-media-query-change', {
      bubbles: true, detail: {id, media, matches, listeners, initial}
    }));
  };
  const destroy = id => {
    const entry = queries.get(id);
    if (!entry) return;
    if (entry.handler) entry.media.removeEventListener('change', entry.handler);
    queries.delete(id);
  };
  const live = id => {
    const entry = queries.get(id);
    if (!entry) throw new Error('Media query has been destroyed');
    return entry;
  };
  const commands = {
    create(request) {
      if (typeof window.matchMedia !== 'function') {
        throw new TypeError('Media queries are not supported by this browser');
      }
      if (queries.has(request.id)) throw new Error('Media query already exists');
      const media = window.matchMedia(request.query);
      queries.set(request.id, {media, listeners: new Set(), handler: null});
      return {media: media.media};
    },
    matches(request) {
      return live(request.id).media.matches;
    },
    subscribe(request) {
      const id = request.id;
      const entry = live(id);
      if (!entry.handler) {
        const handler = event => emit(id, event.media, event.matches,
            Array.from(entry.listeners), false);
        entry.media.addEventListener('change', handler);
        entry.handler = handler;
      }
      entry.listeners.add(request.listener);
      if (request.initial) {
        emit(id, entry.media.media, entry.media.matches, [request.listener], true);
      }
    },
    unsubscribe(request) {
      const entry = queries.get(request.id);
      if (!entry) return;
      entry.listeners.delete(request.listener);
      if (!entry.listeners.size && entry.handler) {
        entry.media.removeEventListener('change', entry.handler);
        entry.handler = null;
      }
    },
    destroy(request) {
      destroy(request.id);
    },
    destroyAll(request) {
      request.ids.forEach(destroy);
    }
  };
  window.__webforjMatchMedia = {
    call(request) {
      try {
        if (!Object.hasOwn(commands, request.command)) {
          throw new Error('Unknown media query command');
        }
        const value = commands[request.command](request) ?? null;
        return JSON.stringify({ok: true, value});
      } catch (error) {
        return JSON.stringify({ok: false, message: String(error.message || error)});
      }
    }
  };
})();

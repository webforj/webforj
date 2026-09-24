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
  window.__webforjMatchMedia = {
    call(request) {
      try {
        let value = null;
        const id = request.id;
        let entry = queries.get(id);
        switch (request.command) {
          case 'create': {
            if (typeof window.matchMedia !== 'function') {
              throw new Error('Media queries are not supported by this browser');
            }
            if (entry) throw new Error('Media query already exists');
            const media = window.matchMedia(request.query);
            queries.set(id, {media, listeners: new Set(), handler: null});
            value = {media: media.media};
            break;
          }
          case 'matches':
            if (!entry) throw new Error('Media query has been destroyed');
            value = entry.media.matches;
            break;
          case 'subscribe':
            if (!entry) throw new Error('Media query has been destroyed');
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
            break;
          case 'unsubscribe':
            if (entry) {
              entry.listeners.delete(request.listener);
              if (!entry.listeners.size && entry.handler) {
                entry.media.removeEventListener('change', entry.handler);
                entry.handler = null;
              }
            }
            break;
          case 'destroy':
            destroy(id);
            break;
          case 'destroyAll':
            request.ids.forEach(destroy);
            break;
          default:
            throw new Error('Unknown media query command');
        }
        return JSON.stringify({ok: true, value});
      } catch (error) {
        return JSON.stringify({ok: false, message: String(error.message || error)});
      }
    }
  };
})();

/**
 * webforJ DevTools Reload Client
 *
 * Keeps the open page alive while the development server restarts and reloads it exactly once
 * when the application can serve again. The client is built from small collaborators, each owning
 * one concern, wired together by {@link DevToolsClient}
 *
 * @author Hyyan Abo Fakher
 * @since 25.02
 */
(function () {
  'use strict';

  if (!window.webforjDevToolsConfig || !window.webforjDevToolsConfig.enabled) {
    return;
  }

  /**
   * Writes console messages with the webforJ DevTools badge.
   */
  class DevToolsLogger {
    /**
     * Logs one message under the DevTools badge.
     *
     * @param {string} message the message to log
     */
    log(message) {
      console.log(
        '%cwebforJ DevTools%c ' + message,
        'background: #4c47ff; color: white; padding: 2px 6px; border-radius: 3px; font-weight: bold;',
        'color: inherit;'
      );
    }
  }

  /**
   * Shows and hides the waiting surface of the client, a full page veil that blocks interaction
   * plus a small status toast in the corner.
   */
  class StatusIndicator {
    constructor() {
      /** @type {string} id of the toast element */
      this.statusId = 'webforj-devtools-status';

      /** @type {string} id of the veil element */
      this.veilId = 'webforj-devtools-veil';

      /** @type {string} id of the injected style element */
      this.styleId = 'webforj-devtools-status-style';
    }

    /**
     * Shows the veil and the toast with the given text, reusing them when already present.
     *
     * @param {string} text the toast text
     */
    show(text) {
      const host = document.getElementById(this.statusId) || this.build();
      const label = host.querySelector('[data-webforj-status]');
      if (label) {
        label.textContent = text;
      }
    }

    /**
     * Removes the veil and the toast.
     */
    hide() {
      document.getElementById(this.statusId)?.remove();
      document.getElementById(this.veilId)?.remove();
    }

    /**
     * Builds the style, the veil, and the toast elements.
     *
     * @returns {HTMLElement} the toast element
     */
    build() {
      if (!document.getElementById(this.styleId)) {
        const style = document.createElement('style');
        style.id = this.styleId;
        style.textContent = /* css */`
          @keyframes webforjDevToolsSpin {
            to { transform: rotate(360deg); }
          }

          @keyframes webforjDevToolsIn {
            from { opacity: 0; transform: translateY(10px); }
            to { opacity: 1; transform: none; }
          }

          #webforj-devtools-veil {
            position: fixed;
            inset: 0;
            z-index: 2147483646;
            cursor: progress;
            background: var(--dwc-overlay-background, rgba(17, 20, 30, 0.16));
          }

          #webforj-devtools-status {
            position: fixed;
            bottom: 18px;
            right: 18px;
            z-index: 2147483647;
            display: flex;
            align-items: center;
            gap: 9px;
            padding: 9px 16px;
            border-radius: var(--dwc-border-radius-pill, 999px);
            background: var(--dwc-surface-3, #fff);
            color: var(--dwc-color-default-text, #222);
            font: 500 13px/1 var(--dwc-font-family, system-ui, sans-serif);
            box-shadow: var(--dwc-shadow-l, 0 8px 24px rgba(0, 0, 0, 0.2));
            animation: webforjDevToolsIn 0.22s ease both;
          }

          #webforj-devtools-status .webforj-devtools-spin {
            width: 14px;
            height: 14px;
            border-radius: 50%;
            border: 2px solid rgba(128, 128, 128, 0.3);
            border-top-color: var(--dwc-color-primary, #6c7bff);
            animation: webforjDevToolsSpin 0.7s linear infinite;
          }
        `;
        document.head.appendChild(style);
      }

      if (!document.getElementById(this.veilId)) {
        const veil = document.createElement('div');
        veil.id = this.veilId;
        document.body.appendChild(veil);
      }

      const host = document.createElement('div');
      host.id = this.statusId;

      const spinner = document.createElement('span');
      spinner.className = 'webforj-devtools-spin';

      const label = document.createElement('span');
      label.setAttribute('data-webforj-status', '');

      host.appendChild(spinner);
      host.appendChild(label);
      document.body.appendChild(host);

      return host;
    }
  }

  /**
   * Gates the page's calls to its server channel.
   *
   * While the server restarts, the channel answers the page with teardown instructions, error
   * responses, and termination messages, and the page reacts by tearing itself down. When the
   * server simply goes away, the page instead stays on its own retry path and keeps everything as
   * it is. The gate routes every channel call through a promise the client controls, so the moment
   * a restart begins, both the calls still in flight and every new call fail exactly the way a
   * stopped server fails them, and the page stays on that retry path until the reload brings the
   * new page in.
   */
  class ServerCallGate {
    /**
     * @param {string} channelPath the path fragment that identifies a server channel call
     */
    constructor(channelPath) {
      /** @type {string} */
      this.channelPath = channelPath;

      /** @type {boolean} whether new and in flight calls are being failed */
      this.holding = false;

      /** @type {Array<function(Error): void>} reject functions of the calls in flight */
      this.pendingRejections = [];
    }

    /**
     * Wraps {@code window.fetch} so channel calls pass through the gate. Installed once at
     * startup and left in place for the lifetime of the page.
     */
    install() {
      const gate = this;
      const nativeFetch = window.fetch;

      window.fetch = function (input) {
        if (!gate.isChannelCall(input)) {
          return nativeFetch.apply(window, arguments);
        }

        if (gate.holding) {
          return Promise.reject(new TypeError('Failed to fetch'));
        }

        const call = nativeFetch.apply(window, arguments);
        return new Promise(function (resolve, reject) {
          gate.pendingRejections.push(reject);

          // A settled call leaves the gate, so the pending list only ever holds the calls that a
          // hold still has to fail.
          const settle = function (deliver, value) {
            const index = gate.pendingRejections.indexOf(reject);
            if (index !== -1) {
              gate.pendingRejections.splice(index, 1);
            }

            deliver(value);
          };

          call.then(function (response) {
            if (gate.holding) {
              settle(reject, new TypeError('Failed to fetch'));
            } else {
              settle(resolve, response);
            }
          }, function (error) {
            settle(reject, error);
          });
        });
      };
    }

    /**
     * Starts failing every channel call, including the ones already in flight.
     */
    hold() {
      this.holding = true;
      const rejections = this.pendingRejections.splice(0);
      for (let i = 0; i < rejections.length; i++) {
        rejections[i](new TypeError('Failed to fetch'));
      }
    }

    /**
     * Lets channel calls through again.
     */
    release() {
      this.holding = false;
    }

    /**
     * Decides whether a fetch input targets the server channel of this page.
     *
     * @param {RequestInfo} input the first fetch argument
     * @returns {boolean} whether the call goes to the server channel on this origin
     */
    isChannelCall(input) {
      const url = typeof input === 'string' ? input : (input && input.url) || '';
      try {
        const resolved = new URL(url, window.location.href);
        return resolved.origin === window.location.origin
          && resolved.pathname.indexOf(this.channelPath) !== -1;
      } catch (e) {
        return false;
      }
    }
  }

  /**
   * Owns the websocket to the reload server, including the heartbeat that keeps it alive. The
   * socket reports its lifecycle to the coordinator through the callbacks given at construction
   * and reconnects only when the coordinator asks it to.
   */
  class ReloadSocket {
    /**
     * @param {string} url the reload server websocket url
     * @param {number} heartbeatInterval milliseconds between heartbeat pings
     * @param {DevToolsLogger} logger the client logger
     * @param {{onFirstOpen: function(): void, onReopen: function(): void,
     *          onMessage: function(Object): void,
     *          onDown: function(boolean, boolean, number): void}} callbacks
     *        coordinator callbacks for socket lifecycle events, onDown receives whether this is
     *        the first drop on the page, whether the dropped instance had been open, and the
     *        close code
     */
    constructor(url, heartbeatInterval, logger, callbacks) {
      /** @type {string} */
      this.url = url;

      /** @type {number} */
      this.heartbeatInterval = heartbeatInterval;

      /** @type {DevToolsLogger} */
      this.logger = logger;

      /** @type {{onFirstOpen: function(): void, onReopen: function(): void,
       *          onMessage: function(Object): void,
       *          onDown: function(boolean, boolean, number): void}} */
      this.callbacks = callbacks;

      /** @type {WebSocket|null} */
      this.socket = null;

      /** @type {number|null} */
      this.heartbeatTimer = null;

      /** @type {boolean} whether the socket has ever been lost */
      this.everDown = false;

      /** @type {boolean} whether the socket has ever been open on this page */
      this.everOpened = false;

      /** @type {boolean} whether the current socket instance reached the open state */
      this.instanceOpened = false;

      /** @type {boolean} whether the page is unloading */
      this.leaving = false;
    }

    /**
     * Opens the socket unless it is already open or opening.
     */
    connect() {
      if (this.socket && (this.socket.readyState === WebSocket.CONNECTING
          || this.socket.readyState === WebSocket.OPEN)) {
        return;
      }

      const self = this;

      try {
        this.instanceOpened = false;
        this.socket = new WebSocket(this.url);

        this.socket.onopen = function () {
          self.logger.log('✅ DevTools connection established! Ready to rock 🎸');
          self.instanceOpened = true;
          self.everOpened = true;
          self.startHeartbeat();
          self.socket.send('ping');

          if (self.everDown) {
            self.callbacks.onReopen();
          } else {
            self.callbacks.onFirstOpen();
          }
        };

        this.socket.onmessage = function (event) {
          try {
            self.callbacks.onMessage(JSON.parse(event.data));
          } catch (e) {
            self.logger.log('💥 Message parsing hiccup: ' + e.message);
          }
        };

        this.socket.onclose = function (event) {
          self.stopHeartbeat();
          if (self.leaving) {
            return;
          }

          const firstDrop = !self.everDown;
          self.everDown = true;
          self.callbacks.onDown(firstDrop, self.instanceOpened, event.code);
        };

        this.socket.onerror = function () {
          self.stopHeartbeat();
        };
      } catch (e) {
        this.logger.log('🙅 WebSocket creation failed: ' + e.message);
        const firstDrop = !this.everDown;
        this.everDown = true;
        this.callbacks.onDown(firstDrop, false, 0);
      }
    }

    /**
     * Reports whether the socket is open right now.
     *
     * @returns {boolean} whether the socket is open
     */
    isOpen() {
      return !!this.socket && this.socket.readyState === WebSocket.OPEN;
    }

    /**
     * Updates the heartbeat cadence and restarts the heartbeat with it.
     *
     * @param {number} interval milliseconds between heartbeat pings
     */
    useHeartbeatInterval(interval) {
      this.heartbeatInterval = interval;
      this.startHeartbeat();
    }

    /**
     * Closes the socket quietly because the page is unloading.
     */
    closeForUnload() {
      this.leaving = true;
      this.stopHeartbeat();
      if (this.socket) {
        this.socket.close();
      }
    }

    /**
     * Starts the heartbeat, replacing a running one.
     */
    startHeartbeat() {
      this.stopHeartbeat();
      const self = this;
      this.heartbeatTimer = setInterval(function () {
        if (self.isOpen()) {
          self.socket.send('ping');
        }
      }, this.heartbeatInterval);
    }

    /**
     * Stops the heartbeat.
     */
    stopHeartbeat() {
      if (this.heartbeatTimer) {
        clearInterval(this.heartbeatTimer);
        this.heartbeatTimer = null;
      }
    }
  }

  /**
   * Asks the server whether the application can serve this page.
   *
   * A successful response alone is no proof, during a redeploy the server can answer
   * with a holding or error page, so the body must carry the client bootstrap reference
   * before the application counts as serving.
   */
  class ApplicationProbe {
    /**
     * Fetches the current page and inspects the body.
     *
     * @returns {Promise<boolean>} resolves with whether the application can serve
     */
    check() {
      return fetch(window.location.href, { method: 'GET', cache: 'no-store' })
        .then(function (response) {
          return response.ok ? response.text() : '';
        })
        .then(function (body) {
          return body.indexOf('webapp/webapp.min.js') !== -1
            || body.indexOf('webapp/webapp.js') !== -1;
        })
        .catch(function () {
          return false;
        });
    }
  }

  /**
   * Applies resource updates pushed by the reload server.
   */
  class ResourceUpdater {
    /**
     * @param {DevToolsLogger} logger the client logger
     * @param {function(string): void} reload asks the coordinator for a full reload with the
     *        given reason
     */
    constructor(logger, reload) {
      /** @type {DevToolsLogger} */
      this.logger = logger;

      /** @type {function(string): void} */
      this.reload = reload;
    }

    /**
     * Applies one resource update message.
     *
     * @param {{resourceType: string, path: string, timestamp: number}} message the update
     */
    apply(message) {
      const resourcePath = this.normalizePath(message.path);

      switch (message.resourceType) {
        case 'css':
          this.updateStylesheets(resourcePath, message.timestamp);
          break;
        case 'image':
          this.updateImages(resourcePath, message.timestamp);
          break;
        case 'js':
          this.reload('⚡ JavaScript updated: ' + resourcePath + ' - full refresh incoming!');
          break;
        case 'other':
          this.reload('📝 File modified: ' + resourcePath + ' - refreshing to apply changes!');
          break;
        default:
          this.logger.log('❓ Unrecognized resource type: ' + message.resourceType);
      }
    }

    /**
     * Refreshes every stylesheet link whose url contains the resource path.
     *
     * @param {string} resourcePath the changed resource path
     * @param {number} timestamp cache buster value
     */
    updateStylesheets(resourcePath, timestamp) {
      const links = document.querySelectorAll('link[rel="stylesheet"]');
      const logger = this.logger;
      const normalize = this.normalizePath;
      let updatedCount = 0;

      links.forEach(function (link) {
        const href = normalize(link.href);
        const originalHref = normalize(link.getAttribute('href'));

        if (href.indexOf(resourcePath) !== -1 || originalHref.indexOf(resourcePath) !== -1) {
          const baseHref = href.split('?')[0];
          link.href = baseHref + '?webforj-dev=' + timestamp;
          updatedCount++;
          logger.log('🎨 CSS hot-reloaded: ' + resourcePath);
        }
      });

      if (updatedCount === 0) {
        logger.log('🔍 No stylesheet found matching: ' + resourcePath);
      } else if (updatedCount > 1) {
        logger.log('🎨 Hot-reloaded ' + updatedCount + ' stylesheets for: ' + resourcePath);
      }
    }

    /**
     * Refreshes every image whose url contains the resource path.
     *
     * @param {string} resourcePath the changed resource path
     * @param {number} timestamp cache buster value
     */
    updateImages(resourcePath, timestamp) {
      const images = document.querySelectorAll('img');
      const normalize = this.normalizePath;
      let updatedCount = 0;

      images.forEach(function (img) {
        const src = normalize(img.src);
        const originalSrc = normalize(img.getAttribute('src'));

        if (src.indexOf(resourcePath) !== -1 || originalSrc.indexOf(resourcePath) !== -1) {
          img.src = src.split('?')[0] + '?webforj-dev=' + timestamp;
          updatedCount++;
        }
      });

      if (updatedCount > 0) {
        this.logger.log('🖼️ Refreshed ' + updatedCount + ' image(s): ' + resourcePath);
      } else {
        this.logger.log('🔍 No images found matching: ' + resourcePath);
      }
    }

    /**
     * Normalizes path separators to forward slashes.
     *
     * @param {string} path the path to normalize
     * @returns {string} the normalized path
     */
    normalizePath(path) {
      return path ? path.split('\\').join('/') : '';
    }
  }

  /**
   * Keeps scroll positions across a reload.
   *
   * The window scroll and the scroll of every scrolled element are captured right before the
   * reload and stored in the session. Components render their scrolling containers inside their
   * own roots, so the capture walks into every component root and records each element as a chain
   * of selectors, one per root, that finds it again in the rebuilt page. After the reload each
   * position goes back the moment its target is back and tall enough to hold it, bounded by
   * {@code restoreMaxWaitMs}. A snapshot older than {@code snapshotMaxAgeMs} is discarded, so a
   * later visit does not inherit the scroll of an unrelated reload.
   */
  class ScrollPositionKeeper {
    constructor() {
      /** @type {string} session storage key holding the captured positions */
      this.storageKey = 'webforj-devtools-scroll';

      /** @type {number} milliseconds a captured snapshot stays valid */
      this.snapshotMaxAgeMs = 30000;

      /** @type {number} milliseconds between restore polls */
      this.restorePollInterval = 50;

      /** @type {number} milliseconds before the restore applies whatever has resolved */
      this.restoreMaxWaitMs = 10000;
    }

    /**
     * Captures the window scroll and every scrolled element into the session.
     */
    capture() {
      const entries = [];
      if (window.scrollX !== 0 || window.scrollY !== 0) {
        entries.push({ window: true, top: window.scrollY, left: window.scrollX });
      }

      this.collect(document, [], entries);

      if (entries.length === 0) {
        return;
      }

      try {
        sessionStorage.setItem(this.storageKey,
          JSON.stringify({ capturedAt: Date.now(), entries: entries }));
      } catch (e) {
        // Session storage may be unavailable, the reload then simply starts at the top.
      }
    }

    /**
     * Collects the scrolled elements under one root and descends into every component root
     * below it.
     *
     * @param {Document|ShadowRoot} root the root to scan
     * @param {Array<string>} rootPath the selector chain leading to this root
     * @param {Array<Object>} entries receives one entry per scrolled element
     */
    collect(root, rootPath, entries) {
      const elements = root.querySelectorAll('*');
      for (let i = 0; i < elements.length; i++) {
        const element = elements[i];
        if (element.scrollTop > 0 || element.scrollLeft > 0) {
          const selector = this.selectorWithin(element);
          if (selector) {
            entries.push({
              path: rootPath.concat([selector]),
              top: element.scrollTop,
              left: element.scrollLeft
            });
          }
        }

        if (element.shadowRoot) {
          const hostSelector = this.selectorWithin(element);
          if (hostSelector) {
            this.collect(element.shadowRoot, rootPath.concat([hostSelector]), entries);
          }
        }
      }
    }

    /**
     * Restores the captured positions into the rebuilt page, then forgets them.
     */
    restore() {
      let snapshot = null;
      try {
        const raw = sessionStorage.getItem(this.storageKey);
        if (!raw) {
          return;
        }

        sessionStorage.removeItem(this.storageKey);
        snapshot = JSON.parse(raw);
      } catch (e) {
        return;
      }

      if (!snapshot || !snapshot.entries
          || Date.now() - snapshot.capturedAt > this.snapshotMaxAgeMs) {
        return;
      }

      const keeper = this;
      let pending = snapshot.entries;
      const deadline = Date.now() + this.restoreMaxWaitMs;

      // The rebuilt page grows into its scroll targets, an element can be back long before its
      // content makes it tall enough to hold the captured position, and a component may reset its
      // own scroll while it finishes rendering. Every entry therefore applies as soon as its
      // target can take the position and keeps reapplying until a later look finds the position
      // in place. Whatever never settles is applied one last time when the wait runs out.
      const poll = function () {
        const expired = Date.now() >= deadline;
        pending = pending.filter(function (entry) {
          if (keeper.hasSettled(entry)) {
            return false;
          }

          if (expired || keeper.canApply(entry)) {
            keeper.apply(entry);
          }

          return !expired;
        });

        if (pending.length > 0) {
          setTimeout(poll, keeper.restorePollInterval);
        }
      };

      setTimeout(poll, this.restorePollInterval);
    }

    /**
     * Reports whether one captured entry already sits at its position.
     *
     * @param {Object} entry the captured entry
     * @returns {boolean} whether the target holds the captured position
     */
    hasSettled(entry) {
      if (entry.window) {
        return Math.abs(window.scrollY - entry.top) <= 1;
      }

      const element = this.resolve(entry.path);

      return !!element && Math.abs(element.scrollTop - entry.top) <= 1;
    }

    /**
     * Resolves one captured selector chain in the current page. Every selector but the last
     * names a component host whose root the chain descends into.
     *
     * @param {Array<string>} path the selector chain
     * @returns {Element|null} the element, null while any step is still missing
     */
    resolve(path) {
      let scope = document;
      let element = null;
      for (let i = 0; i < path.length; i++) {
        element = scope.querySelector(path[i]);
        if (!element) {
          return null;
        }

        if (i < path.length - 1) {
          scope = element.shadowRoot;
          if (!scope) {
            return null;
          }
        }
      }

      return element;
    }

    /**
     * Reports whether one captured entry can take its position, meaning the target is back and
     * tall enough to hold it.
     *
     * @param {Object} entry the captured entry
     * @returns {boolean} whether the position would stick when applied now
     */
    canApply(entry) {
      if (entry.window) {
        return document.documentElement.scrollHeight >= entry.top + window.innerHeight;
      }

      const element = this.resolve(entry.path);

      return !!element && element.scrollHeight >= entry.top + element.clientHeight;
    }

    /**
     * Applies one captured position to the window or to its element. The position lands in one
     * step, overriding any smooth scrolling the target declares, so the settled check reads the
     * final position instead of an animation frame on the way there.
     *
     * @param {Object} entry the captured entry
     */
    apply(entry) {
      if (entry.window) {
        window.scrollTo({ top: entry.top, left: entry.left, behavior: 'instant' });

        return;
      }

      const element = this.resolve(entry.path);
      if (element) {
        element.scrollTo({ top: entry.top, left: entry.left, behavior: 'instant' });
      }
    }

    /**
     * Builds a selector that finds the element again within its own root. The selector climbs
     * from the element toward the top of its root and stops early at the first ancestor carrying
     * an id, so a stable structure resolves to the same element after the rebuild.
     *
     * @param {Element} element the element to describe
     * @returns {string} the selector, empty when none could be built
     */
    selectorWithin(element) {
      const steps = [];
      for (let current = element; current !== null; current = current.parentElement) {
        if (current === document.body || current === document.documentElement) {
          break;
        }

        if (current.id) {
          steps.push('#' + CSS.escape(current.id));
          break;
        }

        steps.push(this.stepFor(current));
      }

      return steps.reverse().join(' > ');
    }

    /**
     * Describes one element by its tag and its position among the same tags before it, which is
     * how the rebuilt page is asked for it again.
     *
     * @param {Element} element the element to describe
     * @returns {string} the selector step for this element
     */
    stepFor(element) {
      let position = 1;
      for (let peer = element.previousElementSibling; peer !== null;
          peer = peer.previousElementSibling) {
        if (peer.localName === element.localName) {
          position++;
        }
      }

      return element.localName + ':nth-of-type(' + position + ')';
    }
  }

  /**
   * Coordinates the whole client.
   *
   * The page must never be torn down by a restart. The moment a restart announces itself, either
   * through the restarting notice or through the reload socket dropping, the coordinator holds
   * the server channel and shows the waiting surface. From then on one recovery loop runs socket
   * reconnection and application probing together on a tight cadence and reloads the page exactly
   * once, as soon as the application proves it can serve:
   *
   * - Socket open and the application answers {@code reloadStableProbes} probes in a row, the
   *   normal restart. The run of probes collapses a build that restarts the server several times
   *   into a single reload.
   * - Socket still down past {@code socketDownGraceMs} while the application answers the same run
   *   of probes. The reload server always comes up before the application can serve, so a serving
   *   application with no socket means the reload server is gone for this deployment and waiting
   *   for it would strand the page on a dead session. The grace keeps the short gap of a normal
   *   restart from being mistaken for that.
   *
   * When nothing answers within {@code recoveryMaxWaitMs} one final probe decides. An
   * application that serves without the socket keeps the page, the hold releases and the page
   * continues against the live server. A dead application means the page would only pretend to
   * work, so the reload goes through and the browser reports the server state.
   */
  class DevToolsClient {
    /**
     * @param {Object} config the injected client configuration
     */
    constructor(config) {
      /** @type {number} milliseconds between recovery ticks */
      this.recoveryInterval = config.probeInterval || 400;

      /** @type {number} milliseconds before a recovery with no serving application gives up */
      this.recoveryMaxWaitMs = config.reconnectMaxWaitMs || 120000;

      /** @type {number} probes the application must answer in a row before the reload */
      this.reloadStableProbes = config.reloadStableProbes || 3;

      /** @type {number} milliseconds the socket must stay down before a serving application
       *  counts without it */
      this.socketDownGraceMs = config.socketDownGraceMs || 3000;

      /** @type {number} milliseconds before a restarting notice with no restart releases */
      this.holdReleaseFallbackMs = config.holdReleaseFallbackMs || 15000;

      /** @type {DevToolsLogger} */
      this.logger = new DevToolsLogger();

      /** @type {StatusIndicator} */
      this.status = new StatusIndicator();

      /** @type {ServerCallGate} */
      this.gate = new ServerCallGate('/webapprmi');

      /** @type {ApplicationProbe} */
      this.probe = new ApplicationProbe();

      /** @type {ResourceUpdater} */
      this.resources = new ResourceUpdater(this.logger, this.reload.bind(this));

      /** @type {ScrollPositionKeeper} */
      this.scroll = new ScrollPositionKeeper();

      /** @type {ReloadSocket} */
      this.socket = new ReloadSocket(config.websocketUrl, config.heartbeatInterval || 30000,
        this.logger, {
          onFirstOpen: this.handleFirstOpen.bind(this),
          onReopen: this.handleReopen.bind(this),
          onMessage: this.handleMessage.bind(this),
          onDown: this.handleSocketDown.bind(this)
        });

      /** @type {boolean} whether the recovery loop is running */
      this.recovering = false;

      /** @type {number} epoch milliseconds when the recovery started */
      this.recoveryStartedAt = 0;

      /** @type {number} epoch milliseconds when the socket last went down */
      this.socketDownSince = 0;

      /** @type {number} probes the application has answered in a row while eligible */
      this.consecutiveReadyProbes = 0;

      /** @type {number|null} */
      this.recoveryTimer = null;

      /** @type {number|null} */
      this.holdReleaseTimer = null;

      /** @type {boolean} whether the reload has been issued */
      this.reloading = false;

      /** @type {string} id of the server side page progress bar */
      this.serverProgressbarId = 'dwc-page-progressbar';
    }

    /**
     * Installs the gate, hides the server progress bar, restores the scroll of the page a reload
     * replaced, connects the socket, and registers the unload cleanup.
     */
    start() {
      this.gate.install();
      this.hideServerProgressbar();
      this.scroll.restore();
      this.logger.log('🚀 Initiating DevTools connection to: ' + this.socket.url);
      this.socket.connect();

      const self = this;
      window.addEventListener('beforeunload', function () {
        self.stopRecovery();
        self.socket.closeForUnload();
      });
    }

    /**
     * Handles the very first socket open of this page.
     */
    handleFirstOpen() {
      // Nothing beyond the handshake, the page is live and untouched.
    }

    /**
     * Handles the socket coming back while a recovery runs.
     */
    handleReopen() {
      this.logger.log('🔌 Server socket is back. Waiting until the app can serve before reloading...');
      this.status.show('Waiting for the app…');
      this.socketDownSince = 0;
    }

    /**
     * Handles one message from the reload server.
     *
     * @param {Object} message the parsed message
     */
    handleMessage(message) {
      switch (message.type) {
        case 'reload':
          this.reload('🎯 Hot reload triggered! Refreshing in 3... 2... 1...');
          break;

        case 'config':
          if (message.heartbeatInterval) {
            this.socket.useHeartbeatInterval(message.heartbeatInterval);
          }
          break;

        case 'heartbeat-ack':
          break;

        case 'connected':
          this.logger.log('🤝 Handshake complete - DevTools is listening for changes!');
          break;

        case 'restarting':
          this.logger.log('♻️ Server is restarting - holding the page until it is back...');
          this.gate.hold();
          this.status.show('Server restarting…');
          this.armHoldReleaseFallback();
          break;

        case 'resource-update':
          this.logger.log('📦 Incoming update: ' + message.resourceType + ' → ' + message.path);
          this.resources.apply(message);
          break;

        default:
          this.logger.log('🤔 Received mystery message type: ' + message.type);
      }
    }

    /**
     * Handles the socket going down. Failed reconnection attempts arrive here as well, so the
     * down timestamp is stamped only on the transition into the down state and the probe run is
     * reset only when a live socket dropped, which means the server went down again.
     *
     * @param {boolean} firstDrop whether this is the first drop since the page loaded
     * @param {boolean} wasOpen whether the dropped socket instance had been open
     * @param {number} code the websocket close code
     */
    handleSocketDown(firstDrop, wasOpen, code) {
      if (firstDrop) {
        this.logger.log('📡 Connection closed (code: ' + code + ') - standing by until the app is back...');
      }

      this.gate.hold();
      this.status.show('Server restarting…');
      this.disarmHoldReleaseFallback();

      if (this.socketDownSince === 0) {
        this.socketDownSince = Date.now();
      }

      if (wasOpen) {
        this.socketDownSince = Date.now();
        this.consecutiveReadyProbes = 0;
      }

      this.startRecovery();
    }

    /**
     * Starts the recovery loop unless it already runs.
     */
    startRecovery() {
      if (this.recovering) {
        return;
      }

      this.recovering = true;
      this.recoveryStartedAt = Date.now();
      this.scheduleRecoveryTick();
    }

    /**
     * Stops the recovery loop.
     */
    stopRecovery() {
      this.recovering = false;
      if (this.recoveryTimer) {
        clearTimeout(this.recoveryTimer);
        this.recoveryTimer = null;
      }
    }

    /**
     * Schedules the next recovery tick.
     */
    scheduleRecoveryTick() {
      const self = this;
      this.recoveryTimer = setTimeout(function () {
        self.recoveryTick();
      }, this.recoveryInterval);
    }

    /**
     * Runs one recovery tick, reconnecting the socket and probing the application together.
     */
    recoveryTick() {
      if (!this.recovering || this.reloading) {
        return;
      }

      if (Date.now() - this.recoveryStartedAt >= this.recoveryMaxWaitMs) {
        this.giveUp();
        return;
      }

      this.socket.connect();

      const self = this;
      this.probe.check().then(function (serves) {
        if (!self.recovering || self.reloading) {
          return;
        }

        const socketOpen = self.socket.isOpen();

        // The reload without a socket is only legal when the socket existed on this page and
        // went away, otherwise a deployment whose reload server never came up would reload
        // itself over and over.
        const socketDownLongEnough = !socketOpen
          && self.socket.everOpened
          && self.socketDownSince !== 0
          && Date.now() - self.socketDownSince >= self.socketDownGraceMs;

        if (serves && (socketOpen || socketDownLongEnough)) {
          self.consecutiveReadyProbes++;
        } else {
          self.consecutiveReadyProbes = 0;
        }

        if (self.consecutiveReadyProbes >= self.reloadStableProbes) {
          if (socketOpen) {
            self.reload('✅ App is ready, reloading.');
          } else {
            self.reload('🔄 The app serves again but its reload socket is gone, reloading into a fresh session.');
          }
          return;
        }

        self.scheduleRecoveryTick();
      });
    }

    /**
     * Ends a recovery whose wait window closed without a stable application. With the socket
     * open the server is up but the application never settled, so the reload goes through
     * anyway, exactly one. With the socket gone one final probe decides. A serving application
     * means this page loaded without a reachable reload socket and still works against the live
     * server, so the hold releases and the page stays. A dead application means the page would
     * only pretend to work, so the reload goes through and the browser reports the server state.
     */
    giveUp() {
      if (this.socket.isOpen()) {
        this.reload('⏳ App did not become ready in time, reloading anyway.');
        return;
      }

      const self = this;
      this.probe.check().then(function (serves) {
        if (serves) {
          self.stopRecovery();
          self.logger.log('😌 The reload socket has not come back in '
            + Math.round(self.recoveryMaxWaitMs / 1000)
            + 's but the app still serves, resuming against it.');
          self.gate.release();
          self.status.hide();
          return;
        }

        self.reload('💀 Nothing answered in ' + Math.round(self.recoveryMaxWaitMs / 1000)
          + 's, reloading so the browser shows the server state.');
      });
    }

    /**
     * Reloads the page once, logging the reason. The scroll positions are captured right before
     * the reload, so the rebuilt page comes back where the old one was.
     *
     * @param {string} reason the log line explaining the reload
     */
    reload(reason) {
      if (this.reloading) {
        return;
      }

      this.reloading = true;
      this.stopRecovery();
      this.logger.log(reason);
      this.status.show('Reloading…');
      this.scroll.capture();
      window.location.reload();
    }

    /**
     * Arms the release fallback for a restarting notice. A notice is normally followed by the
     * socket dropping while the server goes down. Should the drop never come, the page must not
     * stay held forever, so the hold releases itself after the grace period and the page
     * continues against the live server.
     */
    armHoldReleaseFallback() {
      this.disarmHoldReleaseFallback();
      const self = this;
      this.holdReleaseTimer = setTimeout(function () {
        if (self.gate.holding && self.socket.isOpen()) {
          self.logger.log('🤷 Restart notice was not followed by a restart, resuming.');
          self.gate.release();
          self.status.hide();
        }
      }, this.holdReleaseFallbackMs);
    }

    /**
     * Disarms the release fallback.
     */
    disarmHoldReleaseFallback() {
      if (this.holdReleaseTimer) {
        clearTimeout(this.holdReleaseTimer);
        this.holdReleaseTimer = null;
      }
    }

    /**
     * Hides the server side page progress bar so the client's own waiting surface is the only
     * restart indicator.
     */
    hideServerProgressbar() {
      if (document.getElementById('webforj-devtools-progressbar-style')) {
        return;
      }

      const style = document.createElement('style');
      style.id = 'webforj-devtools-progressbar-style';
      style.textContent = '#' + this.serverProgressbarId + '{display:none!important}';
      (document.head || document.documentElement).appendChild(style);
    }
  }

  new DevToolsClient(window.webforjDevToolsConfig).start();
})();

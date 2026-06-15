/**
 * webforJ DevTools Reload Client
 *
 * @author Hyyan Abo Fakher
 * @since 25.02
 */
(function () {
  'use strict';

  if (!window.webforjDevToolsConfig || !window.webforjDevToolsConfig.enabled) {
    return;
  }

  const config = window.webforjDevToolsConfig;
  const wsUrl = config.websocketUrl;
  const heartbeatInterval = config.heartbeatInterval || 30000;
  // A dev redeploy is back in a few seconds on localhost, so poll tight at a fixed interval
  // instead of backing off, and give up by elapsed time.
  const reconnectInterval = config.reconnectInterval || 400;
  const reconnectMaxWaitMs = config.reconnectMaxWaitMs || 120000;
  const probeInterval = config.probeInterval || 400;
  const probeMaxWaitMs = config.probeMaxWaitMs || 120000;
  const serverProgressbarId = 'dwc-page-progressbar';

  let ws;
  let heartbeatTimer;
  let reconnectTimer;
  let reconnectAttempts = 0;
  let hasDisconnected = false;
  let pendingReload = false;
  let disconnectedAt = 0;
  let probeStartedAt = 0;
  let progressbarObserver;

  function log(message) {
    console.log(
      '%cwebforJ DevTools%c ' + message,
      'background: #4c47ff; color: white; padding: 2px 6px; border-radius: 3px; font-weight: bold;',
      'color: inherit;'
    );
  }

  function connect() {
    if (ws && (ws.readyState === WebSocket.CONNECTING || ws.readyState === WebSocket.OPEN)) {
      return;
    }

    if (!hasDisconnected) {
      log('🚀 Initiating DevTools connection to: ' + wsUrl);
    }

    try {
      ws = new WebSocket(wsUrl);

      ws.onopen = function () {
        log('✅ DevTools connection established! Ready to rock 🎸');

        // The reload socket coming back does not mean the application can serve yet. The socket
        // binds early in a redeploy, before the app is initialized and the bundle is in place, so
        // wait until the page actually answers with its bootstrap before reloading.
        if (hasDisconnected && reconnectAttempts > 0) {
          log('🔌 Server socket is back. Waiting until the app can serve before reloading...');
          showStatus('Waiting for the app…');
          reloadWhenReady();
          return;
        }

        reconnectAttempts = 0;
        disconnectedAt = 0;
        startHeartbeat();
        // Send initial ping to confirm connection
        ws.send('ping');
      };

      ws.onmessage = function (event) {
        try {
          const message = JSON.parse(event.data);

          switch (message.type) {
            case 'reload':
              log('🎯 Hot reload triggered! Refreshing in 3... 2... 1...');
              showStatus('Reloading…');
              window.location.reload();
              break;

            case 'config':
              if (message.heartbeatInterval) {
                config.heartbeatInterval = message.heartbeatInterval;
                restartHeartbeat();
              }
              break;

            case 'heartbeat-ack':
              // Heartbeat acknowledged
              break;

            case 'connected':
              // Server handshake confirmation
              log('🤝 Handshake complete - DevTools is listening for changes!');
              break;

            case 'resource-update':
              log('📦 Incoming update: ' + message.resourceType + ' → ' + message.path);
              handleResourceUpdate(message);
              break;

            default:
              log('🤔 Received mystery message type: ' + message.type);
          }
        } catch (e) {
          log('💥 Message parsing hiccup: ' + e.message);
        }
      };

      ws.onclose = function (event) {
        if (!hasDisconnected) {
          log('📡 Connection closed (code: ' + event.code
            + ') - standing by until the app is back...');
          showStatus('Server restarting…');
        }
        hasDisconnected = true;
        stopHeartbeat();
        scheduleReconnect();
      };

      ws.onerror = function () {
        stopHeartbeat();
      };

    } catch (e) {
      log('🙅 WebSocket creation failed: ' + e.message);
      scheduleReconnect();
    }
  }

  function scheduleReconnect() {
    if (disconnectedAt === 0) {
      disconnectedAt = Date.now();
    }

    if (Date.now() - disconnectedAt >= reconnectMaxWaitMs) {
      log('😔 The app has not come back in ' + Math.round(reconnectMaxWaitMs / 1000)
        + 's, giving up. Reload the page to reconnect.');
      hideStatus();
      return;
    }

    if (reconnectTimer) {
      clearTimeout(reconnectTimer);
    }

    // Fixed tight interval so a quick redeploy is caught within a fraction of a second instead of
    // an exponential backoff that overshoots the few seconds a redeploy actually takes.
    reconnectAttempts++;
    reconnectTimer = setTimeout(function () {
      connect();
    }, reconnectInterval);
  }

  // Reloads only once the application can actually serve.
  // A 200 alone can be a holding or error page during a redeploy, so the page
  // body must carry the client bootstrap reference before the reload is allowed through.
  function reloadWhenReady() {
    if (pendingReload) {
      return;
    }

    pendingReload = true;
    probeStartedAt = Date.now();
    probeApp();
  }

  function probeApp() {
    fetch(window.location.href, { method: 'GET', cache: 'no-store' })
      .then(function (response) {
        return response.ok ? response.text() : Promise.reject(new Error('not ready'));
      })
      .then(function (body) {
        if (body.indexOf('webapp/webapp.min.js') !== -1
          || body.indexOf('webapp/webapp.js') !== -1) {
          log('✅ App is ready, reloading.');
          showStatus('Reloading…');
          window.location.reload();
        } else {
          scheduleProbe();
        }
      })
      .catch(function () {
        scheduleProbe();
      });
  }

  function scheduleProbe() {
    if (Date.now() - probeStartedAt >= probeMaxWaitMs) {
      log('⏳ App did not become ready in time, reloading anyway.');
      window.location.reload();
      return;
    }

    // Fixed tight interval, matched to the reconnect cadence.
    setTimeout(probeApp, probeInterval);
  }

  function startHeartbeat() {
    stopHeartbeat();

    heartbeatTimer = setInterval(function () {
      if (ws && ws.readyState === WebSocket.OPEN) {
        ws.send('ping');
      }
    }, heartbeatInterval);
  }

  function stopHeartbeat() {
    if (heartbeatTimer) {
      clearInterval(heartbeatTimer);
      heartbeatTimer = null;
    }
  }

  function restartHeartbeat() {
    stopHeartbeat();
    startHeartbeat();
  }

  function normalizePath(path) {
    // Normalize path separators to forward slashes for cross-platform compatibility
    return path ? path.split('\\').join('/') : '';
  }

  function showStatus(text) {
    const host = document.getElementById('webforj-devtools-status') || buildStatus();
    const label = host.querySelector('[data-webforj-status]');
    if (label) {
      label.textContent = text;
    }
  }

  function buildStatus() {
    if (!document.getElementById('webforj-devtools-status-style')) {
      const style = document.createElement('style');
      style.id = 'webforj-devtools-status-style';
      style.textContent = [
        '@keyframes webforjDevToolsSpin{to{transform:rotate(360deg)}}',
        '@keyframes webforjDevToolsIn{from{opacity:0;transform:translateY(10px)}to{opacity:1;transform:none}}',
        '#webforj-devtools-veil{position:fixed;inset:0;z-index:2147483646;cursor:progress;'
          + 'background:var(--dwc-overlay-background,rgba(17,20,30,0.16))}',
        '#webforj-devtools-status{position:fixed;bottom:18px;right:18px;z-index:2147483647;'
          + 'display:flex;align-items:center;gap:9px;padding:9px 16px;'
          + 'border-radius:var(--dwc-border-radius-pill,999px);background:var(--dwc-surface-3,#fff);'
          + 'color:var(--dwc-color-default-text,#222);'
          + 'font:500 13px/1 var(--dwc-font-family,system-ui,sans-serif);'
          + 'box-shadow:var(--dwc-shadow-l,0 8px 24px rgba(0,0,0,0.2));'
          + 'animation:webforjDevToolsIn 0.22s ease both}',
        '#webforj-devtools-status .webforj-devtools-spin{width:14px;height:14px;border-radius:50%;'
          + 'border:2px solid rgba(128,128,128,0.3);'
          + 'border-top-color:var(--dwc-color-primary,#6c7bff);'
          + 'animation:webforjDevToolsSpin 0.7s linear infinite}'
      ].join('');
      document.head.appendChild(style);
    }

    if (!document.getElementById('webforj-devtools-veil')) {
      const veil = document.createElement('div');
      veil.id = 'webforj-devtools-veil';
      document.body.appendChild(veil);
    }

    const host = document.createElement('div');
    host.id = 'webforj-devtools-status';

    const spinner = document.createElement('span');
    spinner.className = 'webforj-devtools-spin';

    const label = document.createElement('span');
    label.setAttribute('data-webforj-status', '');

    host.appendChild(spinner);
    host.appendChild(label);
    document.body.appendChild(host);

    return host;
  }

  function hideStatus() {
    document.getElementById('webforj-devtools-status')?.remove();
    document.getElementById('webforj-devtools-veil')?.remove();
  }

  // The dwc client raises its own page progress bar the moment it loses the server, ahead of
  // the reload socket noticing. Treat that as another disconnect signal: take the bar down and
  // bring up the reload pill so a redeploy always shows the same single indicator.
  function takeOverServerProgressbar() {
    const bar = document.getElementById(serverProgressbarId);
    if (!bar) {
      return;
    }

    bar.remove();
    showStatus('Server restarting…');
  }

  function watchForServerProgressbar() {
    if (progressbarObserver || typeof MutationObserver === 'undefined' || !document.body) {
      return;
    }

    progressbarObserver = new MutationObserver(takeOverServerProgressbar);
    progressbarObserver.observe(document.body, { childList: true });
    // It may already be on the page if it appeared before this started watching.
    takeOverServerProgressbar();
  }

  function handleResourceUpdate(message) {
    // The path comes from the server as the resource path (e.g., "css/style.css")
    const resourcePath = normalizePath(message.path);

    switch (message.resourceType) {
      case 'css':
        updateStylesheets(resourcePath, message.timestamp);
        break;
      case 'image':
        updateImages(resourcePath, message.timestamp);
        break;
      case 'js':
        // For JavaScript changes, trigger full reload
        log('⚡ JavaScript updated: ' + resourcePath + ' - full refresh incoming!');
        showStatus('Reloading…');
        window.location.reload();
        break;
      case 'other':
        // For other file types, trigger full reload
        log('📝 File modified: ' + resourcePath + ' - refreshing to apply changes!');
        showStatus('Reloading…');
        window.location.reload();
        break;
      default:
        log('❓ Unrecognized resource type: ' + message.resourceType);
    }
  }

  function updateStylesheets(resourcePath, timestamp) {
    // Find all stylesheets
    const links = document.querySelectorAll('link[rel="stylesheet"]');
    let updatedCount = 0;

    links.forEach(function (link) {
      // Check if href contains the resource path
      const href = normalizePath(link.href);
      const originalHref = normalizePath(link.getAttribute('href'));

      // Check if the URL contains the resource path
      if (href.indexOf(resourcePath) !== -1 || originalHref.indexOf(resourcePath) !== -1) {
        // Refresh the link with cache buster
        const baseHref = href.split('?')[0];
        link.href = baseHref + '?webforj-dev=' + timestamp;
        updatedCount++;
        log('🎨 CSS hot-reloaded: ' + resourcePath);
      }
    });

    if (updatedCount === 0) {
      log('🔍 No stylesheet found matching: ' + resourcePath);
    } else if (updatedCount > 1) {
      log('🎨 Hot-reloaded ' + updatedCount + ' stylesheets for: ' + resourcePath);
    }
  }

  function updateImages(resourcePath, timestamp) {
    // Update img elements
    const images = document.querySelectorAll('img');
    let updatedCount = 0;

    images.forEach(function (img) {
      const src = normalizePath(img.src);
      const originalSrc = normalizePath(img.getAttribute('src'));

      if (src.indexOf(resourcePath) !== -1 || originalSrc.indexOf(resourcePath) !== -1) {
        const newSrc = src.split('?')[0] + '?webforj-dev=' + timestamp;
        img.src = newSrc;
        updatedCount++;
      }
    });

    if (updatedCount > 0) {
      log('🖼️ Refreshed ' + updatedCount + ' image(s): ' + resourcePath);
    } else {
      log('🔍 No images found matching: ' + resourcePath);
    }
  }

  // Start connection
  connect();
  watchForServerProgressbar();

  // Clean up on page unload
  window.addEventListener('beforeunload', function () {
    stopHeartbeat();
    if (reconnectTimer) {
      clearTimeout(reconnectTimer);
    }

    if (progressbarObserver) {
      progressbarObserver.disconnect();
    }

    if (ws) {
      ws.close();
    }
  });
})();

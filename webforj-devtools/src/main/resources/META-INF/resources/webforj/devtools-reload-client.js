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

  let ws;
  let heartbeatTimer;
  let reconnectTimer;
  let reconnectAttempts = 0;
  let hasDisconnected = false;
  let pendingReload = false;
  let disconnectedAt = 0;
  let probeStartedAt = 0;

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
          showProgressBar();
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
              showProgressBar();
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
          showProgressBar();
        }
        hasDisconnected = true;
        stopHeartbeat();
        scheduleReconnect();
      };

      ws.onerror = function () {
        log('⚠️ WebSocket hiccup detected: ' + error);
        console.error('[webforJ DevTools] WebSocket error details:', error);
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

  function showProgressBar() {
    // Remove any existing progress bar
    const existing = document.getElementById('webforj-devtools-progressbar');
    if (existing) {
      existing.remove();
    }

    // Remove any existing style
    const existingStyle = document.getElementById('webforj-devtools-progress-style');
    if (existingStyle) {
      existingStyle.remove();
    }

    // Create progress bar container
    const progressBar = document.createElement('div');
    progressBar.id = 'webforj-devtools-progressbar';

    // Create the pulse bar
    const pulseBar = document.createElement('div');

    // Apply styles
    Object.assign(progressBar.style, {
      width: '100%',
      position: 'fixed',
      top: '0',
      left: '0',
      zIndex: '10000',
      height: '3px',
      backgroundColor: 'var(--dwc-color-primary, #4c47ff)',
      overflow: 'hidden'
    });

    Object.assign(pulseBar.style, {
      width: '100%',
      height: '100%',
      backgroundColor: 'rgba(255, 255, 255, 0.7)',
      animation: 'webforjDevToolsPulse 1.5s infinite'
    });

    // Add animation keyframes
    const style = document.createElement('style');
    style.id = 'webforj-devtools-progress-style';
    style.textContent = `
      @keyframes webforjDevToolsPulse {
        0% {
          transform: translateX(-100%);
          opacity: 0;
        }
        50% {
          opacity: 1;
        }
        100% {
          transform: translateX(100%);
          opacity: 0;
        }
      }
    `;
    document.head.appendChild(style);

    // Assemble and add to page
    progressBar.appendChild(pulseBar);
    document.body.appendChild(progressBar);
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
        showProgressBar();
        window.location.reload();
        break;
      case 'other':
        // For other file types, trigger full reload
        log('📝 File modified: ' + resourcePath + ' - refreshing to apply changes!');
        showProgressBar();
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

  // Clean up on page unload
  window.addEventListener('beforeunload', function () {
    stopHeartbeat();
    if (reconnectTimer) {
      clearTimeout(reconnectTimer);
    }
    if (ws) {
      ws.close();
    }
  });
})();

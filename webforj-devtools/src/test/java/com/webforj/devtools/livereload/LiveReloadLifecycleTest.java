package com.webforj.devtools.livereload;

import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;

import java.io.IOException;
import java.net.ServerSocket;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.Timeout;

class LiveReloadLifecycleTest {

  @Test
  void shouldNotStartWhenDisabled() {
    LiveReloadLifecycle lifecycle = new LiveReloadLifecycle();

    lifecycle.start(new LiveReloadOptions().setEnabled(false));

    assertFalse(lifecycle.isRunning());
  }

  @Test
  @Timeout(10)
  void shouldStartAndStopWhenEnabled() throws IOException {
    LiveReloadLifecycle lifecycle = new LiveReloadLifecycle();
    try {
      lifecycle.start(new LiveReloadOptions().setEnabled(true).setWebsocketPort(freePort()));

      assertTrue(lifecycle.isRunning());
    } finally {
      lifecycle.stop();
    }

    assertFalse(lifecycle.isRunning());
  }

  @Test
  @Timeout(10)
  void shouldStartIdempotently() throws IOException {
    LiveReloadLifecycle lifecycle = new LiveReloadLifecycle();
    try {
      LiveReloadOptions options =
          new LiveReloadOptions().setEnabled(true).setWebsocketPort(freePort());
      lifecycle.start(options);
      lifecycle.start(options);

      assertTrue(lifecycle.isRunning());
    } finally {
      lifecycle.stop();
    }
  }

  @Test
  void shouldStopHarmlesslyWhenNotStarted() {
    LiveReloadLifecycle lifecycle = new LiveReloadLifecycle();

    lifecycle.stop();

    assertFalse(lifecycle.isRunning());
  }

  private static int freePort() throws IOException {
    try (ServerSocket socket = new ServerSocket(0)) {
      return socket.getLocalPort();
    }
  }
}

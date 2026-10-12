package com.webforj.devtools.livereload;

import static org.junit.jupiter.api.Assertions.assertArrayEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertSame;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.Mockito.doThrow;
import static org.mockito.Mockito.inOrder;
import static org.mockito.Mockito.mockConstruction;
import static org.mockito.Mockito.never;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;

import com.webforj.devtools.livereload.receiver.HotswapAgentReceiver;
import com.webforj.devtools.livereload.receiver.JrebelReceiver;
import com.webforj.devtools.livereload.receiver.LiveReloadReceiver;
import com.webforj.devtools.livereload.receiver.WatchReceiver;
import com.webforj.plugin.foundation.WatchPortFile;
import java.io.IOException;
import java.net.ServerSocket;
import java.net.Socket;
import java.net.URL;
import java.net.URLClassLoader;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.List;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.Timeout;
import org.junit.jupiter.api.io.TempDir;
import org.mockito.InOrder;
import org.mockito.MockedConstruction;

class LiveReloadLifecycleTest {

  @Test
  @Timeout(10)
  void shouldStartAndStopTheJrebelReceiverWithTheLifecycle() throws IOException {
    LiveReloadLifecycle lifecycle = new LiveReloadLifecycle();
    try {
      lifecycle.start(new LiveReloadOptions().setEnabled(true).setWebsocketPort(freePort()));

      assertNotNull(lifecycle.getJrebelReceiver());
      assertNotNull(lifecycle.getHotswapAgentReceiver());
    } finally {
      lifecycle.stop();
    }

    assertNull(lifecycle.getJrebelReceiver());
    assertNull(lifecycle.getHotswapAgentReceiver());
  }

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

  @Test
  void shouldNotifyRestartingHarmlesslyWhenNotStarted() {
    LiveReloadLifecycle lifecycle = new LiveReloadLifecycle();

    lifecycle.notifyRestarting();

    assertFalse(lifecycle.isRunning());
  }

  @Test
  void shouldSendResourceUpdateHarmlesslyWhenNotStarted() {
    LiveReloadLifecycle lifecycle = new LiveReloadLifecycle();

    lifecycle.sendResourceUpdate("css", "app.css");

    assertFalse(lifecycle.isRunning());
  }

  @Test
  @Timeout(10)
  void shouldNotifyRestartingWhileRunning() throws IOException {
    LiveReloadLifecycle lifecycle = new LiveReloadLifecycle();
    try {
      lifecycle.start(new LiveReloadOptions().setEnabled(true).setWebsocketPort(freePort()));

      lifecycle.notifyRestarting();

      assertTrue(lifecycle.isRunning());
    } finally {
      lifecycle.stop();
    }
  }

  @Test
  @Timeout(10)
  void shouldNotifyRestartingOnlyOncePerStart() throws IOException {
    LiveReloadLifecycle lifecycle = new LiveReloadLifecycle();
    try {
      lifecycle.start(new LiveReloadOptions().setEnabled(true).setWebsocketPort(freePort()));

      lifecycle.notifyRestarting();
      lifecycle.notifyRestarting();

      assertTrue(lifecycle.isRunning());
    } finally {
      lifecycle.stop();
    }
  }

  @Test
  @Timeout(10)
  void shouldStopWhenTheWatchEnds(@TempDir Path tmp) throws Exception {
    String projectRoot = tmp.toAbsolutePath().toString();
    Path portFile = WatchPortFile.resolve(projectRoot);
    LiveReloadLifecycle lifecycle = new LiveReloadLifecycle();

    try (ServerSocket watch = new ServerSocket(0)) {
      Files.writeString(portFile, Integer.toString(watch.getLocalPort()), StandardCharsets.UTF_8);
      lifecycle.start(new LiveReloadOptions().setEnabled(true).setWebsocketPort(freePort())
          .setProjectRoot(projectRoot).setStopOnWatchEnd(true));
      watch.accept().close();
      while (lifecycle.isRunning()) {
        Thread.onSpinWait();
      }

      assertFalse(lifecycle.isRunning());
    } finally {
      lifecycle.stop();
      Files.deleteIfExists(portFile);
    }
  }

  @Test
  @Timeout(10)
  void shouldReconnectAfterTheWatchEndsWhenNotStoppingOnWatchEnd(@TempDir Path tmp)
      throws Exception {
    String projectRoot = tmp.toAbsolutePath().toString();
    Path portFile = WatchPortFile.resolve(projectRoot);
    LiveReloadLifecycle lifecycle = new LiveReloadLifecycle();

    try (ServerSocket watch = new ServerSocket(0)) {
      Files.writeString(portFile, Integer.toString(watch.getLocalPort()), StandardCharsets.UTF_8);
      lifecycle.start(new LiveReloadOptions().setEnabled(true).setWebsocketPort(freePort())
          .setProjectRoot(projectRoot).setStopOnWatchEnd(false));
      watch.accept().close();

      try (Socket reconnected = watch.accept()) {
        assertTrue(reconnected.isConnected());
        assertTrue(lifecycle.isRunning());
      }
    } finally {
      lifecycle.stop();
      Files.deleteIfExists(portFile);
    }
  }

  @Test
  @Timeout(10)
  void shouldOpenClassLoaderOnceInEveryReceiver() throws IOException {
    ClassLoader loader = new URLClassLoader(new URL[0]);
    LiveReloadLifecycle lifecycle = new LiveReloadLifecycle();

    try (MockedConstruction<WatchReceiver> watch = mockConstruction(WatchReceiver.class);
        MockedConstruction<JrebelReceiver> jrebel = mockConstruction(JrebelReceiver.class);
        MockedConstruction<HotswapAgentReceiver> hotswap =
            mockConstruction(HotswapAgentReceiver.class)) {
      lifecycle.start(new LiveReloadOptions().setEnabled(true).setWebsocketPort(freePort()));
      lifecycle.openClassLoader(loader);
      lifecycle.openClassLoader(loader);

      for (LiveReloadReceiver receiver : List.of(watch.constructed().get(0),
          jrebel.constructed().get(0), hotswap.constructed().get(0))) {
        verify(receiver, times(1)).openClassLoader(loader);
      }
    } finally {
      lifecycle.stop();
    }
  }

  @Test
  @Timeout(10)
  void shouldNotOpenClassLoaderBeforeTheStart() throws IOException {
    ClassLoader loader = new URLClassLoader(new URL[0]);
    LiveReloadLifecycle lifecycle = new LiveReloadLifecycle();

    try (MockedConstruction<HotswapAgentReceiver> hotswap =
        mockConstruction(HotswapAgentReceiver.class)) {
      lifecycle.openClassLoader(loader);
      lifecycle.start(new LiveReloadOptions().setEnabled(true).setWebsocketPort(freePort()));
      lifecycle.stop();

      HotswapAgentReceiver receiver = hotswap.constructed().get(0);
      verify(receiver, never()).openClassLoader(loader);
      verify(receiver, never()).closeClassLoader(loader);
    }
  }

  @Test
  @Timeout(10)
  void shouldCloseAnOpenClassLoaderOnceInEveryReceiver() throws IOException {
    ClassLoader loader = new URLClassLoader(new URL[0]);
    LiveReloadLifecycle lifecycle = new LiveReloadLifecycle();

    try (MockedConstruction<WatchReceiver> watch = mockConstruction(WatchReceiver.class);
        MockedConstruction<JrebelReceiver> jrebel = mockConstruction(JrebelReceiver.class);
        MockedConstruction<HotswapAgentReceiver> hotswap =
            mockConstruction(HotswapAgentReceiver.class)) {
      lifecycle.start(new LiveReloadOptions().setEnabled(true).setWebsocketPort(freePort()));
      lifecycle.openClassLoader(loader);
      lifecycle.closeClassLoader(loader);
      lifecycle.closeClassLoader(loader);
      lifecycle.stop();

      for (LiveReloadReceiver receiver : List.of(watch.constructed().get(0),
          jrebel.constructed().get(0), hotswap.constructed().get(0))) {
        verify(receiver, times(1)).closeClassLoader(loader);
      }
    }
  }

  @Test
  @Timeout(10)
  void shouldCloseTheOpenClassLoadersBeforeStoppingTheReceivers() throws IOException {
    ClassLoader loader = new URLClassLoader(new URL[0]);
    LiveReloadLifecycle lifecycle = new LiveReloadLifecycle();

    try (MockedConstruction<HotswapAgentReceiver> hotswap =
        mockConstruction(HotswapAgentReceiver.class)) {
      lifecycle.start(new LiveReloadOptions().setEnabled(true).setWebsocketPort(freePort()));
      lifecycle.openClassLoader(loader);
      lifecycle.stop();

      HotswapAgentReceiver receiver = hotswap.constructed().get(0);
      InOrder order = inOrder(receiver);
      order.verify(receiver).closeClassLoader(loader);
      order.verify(receiver).stop();
    }
  }

  @Test
  @Timeout(10)
  void shouldRollBackTheStartWhenReceiverFailsToStart() throws IOException {
    IllegalStateException failure = new IllegalStateException("start failed");
    LiveReloadOptions options =
        new LiveReloadOptions().setEnabled(true).setWebsocketPort(freePort());
    LiveReloadLifecycle lifecycle = new LiveReloadLifecycle();

    try (MockedConstruction<WatchReceiver> watch = mockConstruction(WatchReceiver.class);
        MockedConstruction<JrebelReceiver> jrebel = mockConstruction(JrebelReceiver.class,
            (receiver, context) -> doThrow(failure).when(receiver).start());
        MockedConstruction<HotswapAgentReceiver> hotswap =
            mockConstruction(HotswapAgentReceiver.class)) {
      IllegalStateException thrown =
          assertThrows(IllegalStateException.class, () -> lifecycle.start(options));

      assertSame(failure, thrown);
      assertFalse(lifecycle.isRunning());
      assertNull(lifecycle.getJrebelReceiver());
      verify(watch.constructed().get(0)).stop();
      verify(hotswap.constructed().get(0), never()).start();
    } finally {
      lifecycle.stop();
    }
  }

  @Test
  @Timeout(10)
  void shouldKeepTheStopFailureOfRolledBackStart() throws IOException {
    IllegalStateException startFailure = new IllegalStateException("start failed");
    IllegalStateException stopFailure = new IllegalStateException("stop failed");
    LiveReloadOptions options =
        new LiveReloadOptions().setEnabled(true).setWebsocketPort(freePort());
    LiveReloadLifecycle lifecycle = new LiveReloadLifecycle();

    try (
        MockedConstruction<WatchReceiver> watch = mockConstruction(WatchReceiver.class,
            (receiver, context) -> doThrow(stopFailure).when(receiver).stop());
        MockedConstruction<JrebelReceiver> jrebel = mockConstruction(JrebelReceiver.class,
            (receiver, context) -> doThrow(startFailure).when(receiver).start())) {
      IllegalStateException thrown =
          assertThrows(IllegalStateException.class, () -> lifecycle.start(options));

      assertSame(startFailure, thrown);
      assertArrayEquals(new Throwable[] {stopFailure}, thrown.getSuppressed());
      assertFalse(lifecycle.isRunning());
      verify(watch.constructed().get(0)).stop();
    } finally {
      lifecycle.stop();
    }
  }

  @Test
  @Timeout(10)
  void shouldStopEveryReceiverWhenOneFailsToStop() throws IOException {
    IllegalStateException failure = new IllegalStateException("stop failed");
    LiveReloadLifecycle lifecycle = new LiveReloadLifecycle();

    try (
        MockedConstruction<WatchReceiver> watch = mockConstruction(WatchReceiver.class,
            (receiver, context) -> doThrow(failure).when(receiver).stop());
        MockedConstruction<JrebelReceiver> jrebel = mockConstruction(JrebelReceiver.class);
        MockedConstruction<HotswapAgentReceiver> hotswap =
            mockConstruction(HotswapAgentReceiver.class)) {
      lifecycle.start(new LiveReloadOptions().setEnabled(true).setWebsocketPort(freePort()));

      IllegalStateException thrown = assertThrows(IllegalStateException.class, lifecycle::stop);

      assertSame(failure, thrown);
      assertFalse(lifecycle.isRunning());
      verify(jrebel.constructed().get(0)).stop();
      verify(hotswap.constructed().get(0)).stop();
    }
  }

  private static int freePort() throws IOException {
    try (ServerSocket socket = new ServerSocket(0)) {
      return socket.getLocalPort();
    }
  }
}
